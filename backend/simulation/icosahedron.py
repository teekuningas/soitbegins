import numpy as np
from collections import defaultdict

class IcoMesh:
    """
    Procedurally generated Icosahedron mesh for continuous-space weather simulation.
    Handles vertex deduplication, neighbor tables, and precomputing edge arrays for 
    vectorized graph operations.
    """

    def __init__(self, subdivisions):
        self.subdivisions = subdivisions
        self.positions, self.faces = self._build_mesh()
        self.n_cells = len(self.positions)
        
        self.neighbors = self._build_neighbors()
        self._build_edge_arrays()
        self._compute_cell_areas()
        self._compute_operator_weights()

    def _base_icosahedron(self):
        """Generates the 12 vertices and 20 faces of a base icosahedron."""
        phi = (1.0 + np.sqrt(5.0)) / 2.0
        
        # 12 base vertices
        verts = [
            [-1,  phi,  0], [ 1,  phi,  0], [-1, -phi,  0], [ 1, -phi,  0],
            [ 0, -1,  phi], [ 0,  1,  phi], [ 0, -1, -phi], [ 0,  1, -phi],
            [ phi,  0, -1], [ phi,  0,  1], [-phi,  0, -1], [-phi,  0,  1]
        ]
        
        # Normalize to unit sphere
        verts = np.array(verts, dtype=np.float64)
        verts /= np.linalg.norm(verts, axis=1)[:, np.newaxis]
        
        # 20 faces
        faces = [
            [0, 11, 5], [0, 5, 1], [0, 1, 7], [0, 7, 10], [0, 10, 11],
            [1, 5, 9], [5, 11, 4], [11, 10, 2], [10, 7, 6], [7, 1, 8],
            [3, 9, 4], [3, 4, 2], [3, 2, 6], [3, 6, 8], [3, 8, 9],
            [4, 9, 5], [2, 4, 11], [6, 2, 10], [8, 6, 7], [9, 8, 1]
        ]
        
        return verts.tolist(), faces

    def _subdivide(self, verts, faces):
        """Subdivides each triangle into 4 smaller triangles, caching midpoints to avoid duplicates."""
        new_faces = []
        midpoint_cache = {}
        
        def get_midpoint(v1_idx, v2_idx):
            key = (min(v1_idx, v2_idx), max(v1_idx, v2_idx))
            if key in midpoint_cache:
                return midpoint_cache[key]
            
            # Calculate midpoint
            v1 = np.array(verts[v1_idx])
            v2 = np.array(verts[v2_idx])
            mp = (v1 + v2) / 2.0
            
            # Project to unit sphere
            mp /= np.linalg.norm(mp)
            
            new_idx = len(verts)
            verts.append(mp.tolist())
            midpoint_cache[key] = new_idx
            return new_idx

        for face in faces:
            v0, v1, v2 = face
            
            a = get_midpoint(v0, v1)
            b = get_midpoint(v1, v2)
            c = get_midpoint(v2, v0)
            
            new_faces.extend([
                [v0, a, c],
                [v1, b, a],
                [v2, c, b],
                [a, b, c]
            ])
            
        return verts, new_faces

    def _build_mesh(self):
        verts, faces = self._base_icosahedron()
        for _ in range(self.subdivisions):
            verts, faces = self._subdivide(verts, faces)
        return np.array(verts), np.array(faces)

    def _build_neighbors(self):
        """Builds adjacency list for each vertex based on faces."""
        neighbors = defaultdict(set)
        for face in self.faces:
            v0, v1, v2 = face
            neighbors[v0].update([v1, v2])
            neighbors[v1].update([v0, v2])
            neighbors[v2].update([v0, v1])
            
        # Convert to list of lists for fast iteration
        return [list(neighbors[i]) for i in range(self.n_cells)]

    def _build_edge_arrays(self):
        """
        Builds flattened arrays of edges for fast vectorized scatter/gather operations.
        Each edge is directional (src -> dst).
        """
        src_list, dst_list = [], []
        tangent_list, dist_list = [], []
        
        for i in range(self.n_cells):
            pos_i = self.positions[i]
            for j in self.neighbors[i]:
                pos_j = self.positions[j]
                
                # Raw edge vector from i to j
                raw_edge = pos_j - pos_i
                
                # Project edge to the tangent plane at i
                # tangent = raw_edge - dot(raw_edge, normal) * normal
                tangent = raw_edge - np.dot(raw_edge, pos_i) * pos_i
                
                dist = np.linalg.norm(tangent)
                if dist > 0:
                    src_list.append(i)
                    dst_list.append(j)
                    tangent_list.append(tangent / dist)
                    dist_list.append(dist)
                    
        self.edge_src = np.array(src_list)
        self.edge_dst = np.array(dst_list)
        self.edge_tangent = np.array(tangent_list)  # Shape (n_edges, 3)
        self.edge_dist = np.array(dist_list)
        
        self.neighbor_count = np.array([len(n) for n in self.neighbors])

    def _compute_cell_areas(self):
        """Approximate Voronoi cell areas (uniform assumption)."""
        self.cell_areas = np.full(self.n_cells, 4 * np.pi / self.n_cells)

    def _compute_operator_weights(self):
        """Precompute weights for gradient and advection operators.

        On a regular grid, central difference divides by 2 (two neighbors per axis).
        On an isotropic N-neighbor mesh, the identity  Σ ê⊗ê = (N/2)I  gives:

            gradient  = (2/N) × Σ_all     (F_j − F_i) ê_ij
            advection = (4/N) × Σ_upwind   w_ij (F_j − F_i)

        The factor 2 between them: upwind uses half the stencil.
        sin_lat = z-coordinate on the unit sphere = sin(latitude).
        """
        self.sin_lat = self.positions[:, 2]
        self.gradient_weight = 2.0 / self.neighbor_count
        self.advection_weight = 2.0 * self.gradient_weight
