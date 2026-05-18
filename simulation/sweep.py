"""
Parameter Sweep Framework
=========================

Runs the simulation in-process (no ZMQ) with varying parameters to find
optimal configurations for gameplay wind diversity.

Usage:
    python sweep.py                          # Default sweep of all params
    python sweep.py solar 0.05 0.5 10        # Sweep solar from 0.05 to 0.5 in 10 steps
    python sweep.py --frames 500             # Run 500 frames per config (default: 300)
    python sweep.py --output results.csv     # Write results to CSV
    python sweep.py --compare                # Compare two param sets side by side

Output metrics per run:
    - Equilibrium energy (thermal, horizontal KE, vertical KE)
    - Convergence frame (when energy stabilizes within 1%)
    - Max wind speed (overall and per-layer)
    - Vertical wind diversity (std of w across layers)
    - Horizontal wind diversity (std of |wind| across cells)
    - Zonal structure score (how much wind varies with latitude)
"""

import sys
import csv
import time
import numpy as np
from dataclasses import asdict
from icosahedron import IcoMesh
from atmosphere_3d import Params, step_atmosphere_3d


def run_simulation(mesh, params, n_frames=300, sun_start=0.0):
    """Run simulation for N frames, return trajectory and final state."""
    T = np.zeros((params.n_layers, mesh.n_cells), dtype=np.float64)
    wind = np.zeros((params.n_layers, mesh.n_cells, 3), dtype=np.float64)
    w = np.zeros((params.n_layers, mesh.n_cells), dtype=np.float64)

    sun_lon = sun_start
    trajectory = []

    for frame in range(n_frames):
        sun_lon += params.dt * params.omega
        sun_dir = np.array(
            [
                np.cos(sun_lon) * np.cos(params.tilt),
                np.sin(sun_lon) * np.cos(params.tilt),
                np.sin(params.tilt),
            ]
        )

        T, wind, w = step_atmosphere_3d(mesh, T, wind, w, params, sun_dir)

        if frame % 10 == 0 or frame == n_frames - 1:
            wind_speed = np.linalg.norm(wind, axis=-1)
            trajectory.append(
                {
                    "frame": frame,
                    "thermal_energy": float(np.sum(T**2)),
                    "horiz_ke": float(np.sum(wind**2)),
                    "vert_ke": float(np.sum(w**2)),
                    "wind_max": float(wind_speed.max()),
                    "wind_mean": float(wind_speed.mean()),
                    "w_max": float(np.abs(w).max()),
                }
            )

    return {
        "trajectory": trajectory,
        "final": {"T": T, "wind": wind, "w": w},
    }


def analyze_run(mesh, result, params):
    """Compute summary metrics from a completed run."""
    traj = result["trajectory"]
    T = result["final"]["T"]
    wind = result["final"]["wind"]
    w = result["final"]["w"]

    # Energy at equilibrium (last 20% of trajectory)
    n_tail = max(1, len(traj) // 5)
    tail = traj[-n_tail:]
    eq_thermal = np.mean([t["thermal_energy"] for t in tail])
    eq_horiz_ke = np.mean([t["horiz_ke"] for t in tail])
    eq_vert_ke = np.mean([t["vert_ke"] for t in tail])
    eq_total = eq_thermal + eq_horiz_ke + eq_vert_ke

    # Convergence frame (first frame where energy is within 5% of equilibrium)
    convergence_frame = traj[-1]["frame"]
    for t in traj:
        total = t["thermal_energy"] + t["horiz_ke"] + t["vert_ke"]
        if abs(total - eq_total) / max(eq_total, 1e-10) < 0.05:
            convergence_frame = t["frame"]
            break

    # Wind diversity metrics (from final state)
    wind_speed = np.linalg.norm(wind, axis=-1)

    # Per-layer max wind
    layer_max_wind = [float(wind_speed[k].max()) for k in range(params.n_layers)]

    # Vertical diversity: how much wind speed varies across layers at each cell
    vert_diversity = float(np.mean(np.std(wind_speed, axis=0)))

    # Horizontal diversity: how much wind speed varies across cells at each layer
    horiz_diversity = float(np.mean(np.std(wind_speed, axis=1)))

    # Zonal structure: correlation of wind speed with latitude
    sin_lat = mesh.sin_lat
    zonal_scores = []
    for k in range(params.n_layers):
        if wind_speed[k].std() > 1e-10:
            corr = abs(np.corrcoef(sin_lat, wind_speed[k])[0, 1])
            zonal_scores.append(corr)
    zonal_score = float(np.mean(zonal_scores)) if zonal_scores else 0.0

    # Vertical reversal: at how many cells does wind direction flip between layers
    reversals = 0
    for k in range(params.n_layers - 1):
        dot = np.einsum("cj,cj->c", wind[k], wind[k + 1])
        reversals += np.sum(dot < 0)
    reversal_fraction = float(reversals) / (mesh.n_cells * (params.n_layers - 1))

    return {
        "eq_thermal": eq_thermal,
        "eq_horiz_ke": eq_horiz_ke,
        "eq_vert_ke": eq_vert_ke,
        "eq_total": eq_total,
        "convergence_frame": convergence_frame,
        "wind_max": float(wind_speed.max()),
        "wind_mean": float(wind_speed.mean()),
        "w_max": float(np.abs(w).max()),
        "vert_diversity": vert_diversity,
        "horiz_diversity": horiz_diversity,
        "zonal_score": zonal_score,
        "reversal_fraction": reversal_fraction,
        "layer_max_wind": layer_max_wind,
    }


def sweep_parameter(param_name, values, n_frames=300, subdivisions=4):
    """Sweep one parameter across given values."""
    mesh = IcoMesh(subdivisions=subdivisions)
    results = []

    print(f"Sweeping {param_name}: {len(values)} values, {n_frames} frames each")
    print(f"Mesh: {mesh.n_cells} cells, subdivision level {subdivisions}")
    print("-" * 80)

    for i, val in enumerate(values):
        params = Params(n_layers=8, tilt=np.radians(23.5))
        setattr(params, param_name, val)

        t0 = time.time()
        result = run_simulation(mesh, params, n_frames)
        dt = time.time() - t0

        metrics = analyze_run(mesh, result, params)
        metrics["param_name"] = param_name
        metrics["param_value"] = val
        results.append(metrics)

        print(
            f"  [{i+1}/{len(values)}] {param_name}={val:.4f}  "
            f"E={metrics['eq_total']:.0f}  "
            f"|wind|_max={metrics['wind_max']:.3f}  "
            f"vert_div={metrics['vert_diversity']:.4f}  "
            f"zonal={metrics['zonal_score']:.3f}  "
            f"reversal={metrics['reversal_fraction']:.3f}  "
            f"({dt:.1f}s)"
        )

    return results


def default_sweeps(n_frames=300, subdivisions=4):
    """Run the standard sensitivity analysis suite."""
    sweeps = {
        "solar": np.linspace(0.05, 0.40, 8),
        "cooling": np.linspace(0.005, 0.08, 8),
        "c_sq": np.linspace(0.05, 0.40, 8),
        "drag": np.linspace(0.005, 0.08, 8),
        "omega": np.linspace(0.1, 1.0, 8),
        "g": np.linspace(0.02, 0.20, 8),
    }

    all_results = []
    for param_name, values in sweeps.items():
        print(f"\n{'='*80}")
        results = sweep_parameter(param_name, values, n_frames, subdivisions)
        all_results.extend(results)
        print()

    return all_results


def export_results(results, filename="sweep_results.csv"):
    """Export sweep results to CSV."""
    if not results:
        print("No results to export.")
        return

    # Flatten layer_max_wind into separate columns
    flat_results = []
    for r in results:
        row = {k: v for k, v in r.items() if k != "layer_max_wind"}
        for i, lmw in enumerate(r.get("layer_max_wind", [])):
            row[f"layer_{i}_max_wind"] = lmw
        flat_results.append(row)

    fieldnames = list(flat_results[0].keys())
    with open(filename, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(flat_results)
    print(f"Results exported to {filename}")


def print_summary(results):
    """Print a summary comparison table."""
    if not results:
        return

    param_name = results[0]["param_name"]
    print(f"\n{'='*80}")
    print(f"  SWEEP SUMMARY: {param_name}")
    print(f"{'='*80}")
    print(
        f"  {'Value':>8}  {'Energy':>8}  {'|wind|max':>10}  "
        f"{'VertDiv':>8}  {'HorizDiv':>9}  {'Zonal':>6}  {'Reversal':>8}"
    )
    print(f"  {'-'*8}  {'-'*8}  {'-'*10}  {'-'*8}  {'-'*9}  {'-'*6}  {'-'*8}")
    for r in results:
        print(
            f"  {r['param_value']:8.4f}  {r['eq_total']:8.0f}  {r['wind_max']:10.4f}  "
            f"{r['vert_diversity']:8.4f}  {r['horiz_diversity']:9.4f}  "
            f"{r['zonal_score']:6.3f}  {r['reversal_fraction']:8.3f}"
        )

    # Find best for gameplay (high diversity, high reversals)
    best = max(results, key=lambda r: r["vert_diversity"] + r["reversal_fraction"])
    print(f"\n  Best for gameplay diversity: {param_name}={best['param_value']:.4f}")
    print(
        f"    (vert_div={best['vert_diversity']:.4f}, "
        f"reversal={best['reversal_fraction']:.3f}, "
        f"|wind|_max={best['wind_max']:.4f})"
    )


def main():
    args = sys.argv[1:]
    n_frames = 300
    subdivisions = 4
    output_file = "sweep_results.csv"

    # Parse flags
    remaining = []
    i = 0
    while i < len(args):
        if args[i] == "--frames" and i + 1 < len(args):
            n_frames = int(args[i + 1])
            i += 2
        elif args[i] == "--output" and i + 1 < len(args):
            output_file = args[i + 1]
            i += 2
        elif args[i] == "--subdivisions" and i + 1 < len(args):
            subdivisions = int(args[i + 1])
            i += 2
        else:
            remaining.append(args[i])
            i += 1

    if not remaining:
        # Default: run all sweeps
        results = default_sweeps(n_frames, subdivisions)
        export_results(results, output_file)
    elif len(remaining) >= 4:
        # Single parameter sweep: name min max steps
        param_name = remaining[0]
        low = float(remaining[1])
        high = float(remaining[2])
        steps = int(remaining[3])
        values = np.linspace(low, high, steps)
        results = sweep_parameter(param_name, values, n_frames, subdivisions)
        print_summary(results)
        export_results(results, output_file)
    elif remaining[0] == "--compare":
        # Quick comparison: default vs recommended params
        mesh = IcoMesh(subdivisions=subdivisions)
        configs = [
            ("default", Params(n_layers=8, tilt=np.radians(23.5))),
            (
                "high_solar",
                Params(n_layers=8, tilt=np.radians(23.5), solar=0.25),
            ),
            (
                "high_omega",
                Params(n_layers=8, tilt=np.radians(23.5), omega=0.8),
            ),
            (
                "tuned",
                Params(
                    n_layers=8,
                    tilt=np.radians(23.5),
                    solar=0.25,
                    cooling=0.02,
                    drag=0.02,
                ),
            ),
        ]
        print(f"Comparing {len(configs)} configurations, {n_frames} frames each:")
        for name, params in configs:
            t0 = time.time()
            result = run_simulation(mesh, params, n_frames)
            metrics = analyze_run(mesh, result, params)
            dt = time.time() - t0
            print(
                f"\n  {name:12s}  E={metrics['eq_total']:.0f}  "
                f"|wind|={metrics['wind_max']:.3f}  "
                f"vert_div={metrics['vert_diversity']:.4f}  "
                f"zonal={metrics['zonal_score']:.3f}  "
                f"reversal={metrics['reversal_fraction']:.3f}  "
                f"({dt:.1f}s)"
            )
            print(
                f"{'':14s}  params: solar={params.solar} cooling={params.cooling} "
                f"drag={params.drag} omega={params.omega} g={params.g}"
            )
    else:
        print(__doc__)
        sys.exit(1)


if __name__ == "__main__":
    main()
