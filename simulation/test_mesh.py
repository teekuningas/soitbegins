import time
from icosahedron import IcoMesh


def run_mesh_diagnostics(subdivisions):
    print(f"\n{'='*50}")
    print(f"Generating Icosahedron (Subdivisions: {subdivisions})")
    print(f"{'='*50}")

    t0 = time.time()
    mesh = IcoMesh(subdivisions=subdivisions)
    t1 = time.time()

    print(f"Generation Time: {t1 - t0:.4f} seconds")
    print(f"Total Vertices:  {mesh.n_cells}")
    print(f"Total Faces:     {len(mesh.faces)}")

    # Expected formula for vertices: 10 * 4^n + 2
    expected_verts = 10 * (4**subdivisions) + 2
    print(
        f"Expected Verts:  {expected_verts} -> {'PASS' if mesh.n_cells == expected_verts else 'FAIL'}"
    )

    # Topology validation
    n_counts = [len(n) for n in mesh.neighbors]
    count_5 = n_counts.count(5)
    count_6 = n_counts.count(6)
    other = mesh.n_cells - count_5 - count_6

    print("\nTopology Check:")
    print(f"Vertices with 5 neighbors (Poles/Defects): {count_5} (Expected: 12)")
    print(
        f"Vertices with 6 neighbors (Hexagons):      {count_6} (Expected: {mesh.n_cells - 12})"
    )
    if other > 0:
        print(f"WARNING: Found {other} vertices with unusual neighbor counts!")

    print("\nVectorized Edges Check:")
    print(f"Total directed edges: {len(mesh.edge_src)}")
    print(f"Average edge distance (radians): {mesh.edge_dist.mean():.6f}")


if __name__ == "__main__":
    # Level 4 is good for quick testing/overview mapping
    run_mesh_diagnostics(subdivisions=4)

    # Level 5 is the target simulation resolution (~10k cells)
    run_mesh_diagnostics(subdivisions=5)
