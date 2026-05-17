import time
import numpy as np
from icosahedron import IcoMesh
from atmosphere import Params, step_atmosphere

def run_physics_test():
    print(f"\n{'='*50}")
    print("Testing Icosahedral Atmosphere Physics")
    print(f"{'='*50}")
    
    # Level 4 is ~2500 cells, perfect for quick iterative testing
    mesh = IcoMesh(subdivisions=4)
    p = Params()
    
    # Initial State
    T = np.zeros(mesh.n_cells, dtype=np.float64)
    wind = np.zeros((mesh.n_cells, 3), dtype=np.float64)
    sun_lon = 0.0
    
    frames = 100
    print(f"Running {frames} simulation steps...")
    
    t0 = time.time()
    for frame in range(frames):
        sun_lon += p.dt * p.omega  # Sun moves with rotation
        T, wind = step_atmosphere(mesh, T, wind, p, sun_lon)
        
        # Energy Diagnostics
        if frame % 25 == 0 or frame == frames - 1:
            thermal = np.sum(T**2)
            kinetic = np.sum(wind**2)
            max_wind = np.sqrt(np.max(np.sum(wind**2, axis=1)))
            print(f"Step {frame:03d} | Thermal E: {thermal:.2f} | Kinetic E: {kinetic:.2f} | Max Wind: {max_wind:.4f}")
            
    t1 = time.time()
    
    print(f"Simulation Complete. Time taken: {t1 - t0:.4f} seconds")
    print(f"Time per frame: {(t1 - t0) / frames * 1000:.2f} ms")
    
if __name__ == "__main__":
    run_physics_test()
