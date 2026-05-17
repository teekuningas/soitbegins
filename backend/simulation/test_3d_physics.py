import time
import numpy as np
from icosahedron import IcoMesh
from atmosphere_3d import Params, step_atmosphere_3d

def run_3d_physics_test():
    print(f"\n{'='*60}")
    print("Testing 3D Multi-Layer Icosahedral Atmosphere")
    print(f"{'='*60}")
    
    subdivisions = 4
    mesh = IcoMesh(subdivisions=subdivisions)
    p = Params(n_layers=8, tilt=np.radians(23.5)) # 23.5 deg solstice tilt
    
    # Initial State (n_layers, n_cells)
    T = np.zeros((p.n_layers, mesh.n_cells), dtype=np.float64)
    wind = np.zeros((p.n_layers, mesh.n_cells, 3), dtype=np.float64)
    w = np.zeros((p.n_layers, mesh.n_cells), dtype=np.float64)
    sun_lon = 0.0
    
    frames = 500
    print(f"Mesh: {mesh.n_cells} cells | Layers: {p.n_layers} | Total State: {mesh.n_cells * p.n_layers} cells")
    print(f"Tilt: 23.5 degrees (Solstice conditions)")
    print(f"Running {frames} simulation steps...")
    
    t0 = time.time()
    for frame in range(1, frames + 1):
        sun_lon += p.dt * p.omega
        T, wind, w = step_atmosphere_3d(mesh, T, wind, w, p, sun_lon)
        
        # Energy Diagnostics
        if frame % 50 == 0 or frame == frames:
            thermal = np.sum(T**2)
            horiz_ke = np.sum(wind**2)
            vert_ke = np.sum(w**2)
            max_wind = np.sqrt(np.max(np.sum(wind**2, axis=2)))
            max_w = np.max(np.abs(w))
            print(f"Step {frame:04d} | Thermal: {thermal:8.2f} | Horiz KE: {horiz_ke:7.2f} | Vert KE: {vert_ke:7.2f} | Max Wind: {max_wind:.3f} | Max Vert: {max_w:.3f}")
            
    t1 = time.time()
    
    print(f"\nSimulation Complete. Time taken: {t1 - t0:.4f} seconds")
    print(f"Time per frame: {(t1 - t0) / frames * 1000:.2f} ms")
    
if __name__ == "__main__":
    run_3d_physics_test()
