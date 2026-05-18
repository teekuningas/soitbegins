"""
Simulation Visualizer
=====================

3D matplotlib visualization of the atmosphere simulation on the icosahedron.
Can run standalone (in-process simulation) or connect to a running server.

Usage:
    python visualize.py                     # Standalone, animate from cold start
    python visualize.py --layer 3           # Show layer 3 (default: 0 = surface)
    python visualize.py --field T           # Color by temperature (default)
    python visualize.py --field wind        # Color by wind speed
    python visualize.py --field w           # Color by vertical velocity
    python visualize.py --arrows            # Show wind direction arrows
    python visualize.py --frames 200        # Animate 200 frames
    python visualize.py --snapshot 100      # Run 100 frames then show static plot
    python visualize.py --remote            # Connect to running simulation server
    python visualize.py --all-layers        # Side-by-side plot of all layers
"""

import sys
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize, TwoSlopeNorm
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
import matplotlib.animation as animation

from icosahedron import IcoMesh
from atmosphere_3d import Params, step_atmosphere_3d


def get_field_data(T, wind, w, field, layer):
    """Extract the scalar field to visualize."""
    if field == "T":
        return T[layer], "Temperature", "RdBu_r"
    elif field == "wind":
        return np.linalg.norm(wind[layer], axis=-1), "Wind Speed", "viridis"
    elif field == "w":
        return w[layer], "Vertical Velocity", "RdBu_r"
    else:
        raise ValueError(f"Unknown field: {field}")


def plot_sphere_scatter(ax, mesh, values, title, cmap="viridis", norm=None):
    """Plot cells as colored scatter points on a sphere."""
    ax.clear()
    pos = mesh.positions
    if norm is None:
        vmin, vmax = values.min(), values.max()
        if vmin < 0 and vmax > 0:
            norm = TwoSlopeNorm(vmin=vmin, vcenter=0, vmax=max(abs(vmin), vmax))
        else:
            norm = Normalize(vmin=vmin, vmax=vmax)

    colors = plt.get_cmap(cmap)(norm(values))
    ax.scatter(
        pos[:, 0], pos[:, 1], pos[:, 2], c=colors, s=8, alpha=0.9, edgecolors="none"
    )
    ax.set_xlim([-1.1, 1.1])
    ax.set_ylim([-1.1, 1.1])
    ax.set_zlim([-1.1, 1.1])
    ax.set_title(title, fontsize=10)
    ax.set_aspect("equal")
    ax.axis("off")


def plot_wind_arrows(ax, mesh, wind_layer, scale=5.0, stride=4):
    """Overlay wind direction arrows on the sphere."""
    pos = mesh.positions[::stride]
    w = wind_layer[::stride]
    speeds = np.linalg.norm(w, axis=-1)
    mask = speeds > 0.001
    if mask.any():
        ax.quiver(
            pos[mask, 0],
            pos[mask, 1],
            pos[mask, 2],
            w[mask, 0] * scale,
            w[mask, 1] * scale,
            w[mask, 2] * scale,
            color="black",
            alpha=0.4,
            arrow_length_ratio=0.3,
            linewidth=0.5,
        )


def plot_sun(ax, sun_dir):
    """Plot the sun position as a yellow marker."""
    ax.scatter(
        [sun_dir[0] * 1.3],
        [sun_dir[1] * 1.3],
        [sun_dir[2] * 1.3],
        color="gold",
        s=200,
        marker="*",
        zorder=10,
    )


def snapshot(
    mesh, T, wind, w, params, field="T", layer=0, show_arrows=False, sun_dir=None
):
    """Static snapshot of the simulation state."""
    values, label, cmap = get_field_data(T, wind, w, field, layer)

    fig = plt.figure(figsize=(10, 8))
    ax = fig.add_subplot(111, projection="3d")
    plot_sphere_scatter(ax, mesh, values, f"{label} — Layer {layer}", cmap)
    if show_arrows:
        plot_wind_arrows(ax, mesh, wind[layer])
    if sun_dir is not None:
        plot_sun(ax, sun_dir)

    plt.tight_layout()
    plt.show()


def all_layers_snapshot(mesh, T, wind, w, params, field="T", sun_dir=None):
    """Plot all layers side by side."""
    n_layers = params.n_layers
    cols = 4
    rows = (n_layers + cols - 1) // cols

    fig = plt.figure(figsize=(4 * cols, 4 * rows))
    fig.suptitle(f"All Layers — {field}", fontsize=14)

    # Compute global color range
    all_values = []
    for k in range(n_layers):
        v, _, _ = get_field_data(T, wind, w, field, k)
        all_values.append(v)
    all_flat = np.concatenate(all_values)
    vmin, vmax = all_flat.min(), all_flat.max()
    if vmin < 0 and vmax > 0:
        norm = TwoSlopeNorm(vmin=vmin, vcenter=0, vmax=max(abs(vmin), vmax))
    else:
        norm = Normalize(vmin=vmin, vmax=vmax)

    for k in range(n_layers):
        ax = fig.add_subplot(rows, cols, k + 1, projection="3d")
        values, label, cmap = get_field_data(T, wind, w, field, k)
        plot_sphere_scatter(ax, mesh, values, f"Layer {k}", cmap, norm)
        if sun_dir is not None:
            plot_sun(ax, sun_dir)

    plt.tight_layout()
    plt.show()


def animate_standalone(
    mesh, params, field="T", layer=0, n_frames=200, show_arrows=False
):
    """Animate the simulation from cold start."""
    T = np.zeros((params.n_layers, mesh.n_cells), dtype=np.float64)
    wind = np.zeros((params.n_layers, mesh.n_cells, 3), dtype=np.float64)
    w = np.zeros((params.n_layers, mesh.n_cells), dtype=np.float64)
    sun_lon = 0.0

    fig = plt.figure(figsize=(10, 8))
    ax = fig.add_subplot(111, projection="3d")

    def update(frame_num):
        nonlocal T, wind, w, sun_lon
        # Step physics 5 times per animation frame for speed
        for _ in range(5):
            sun_lon += params.dt * params.omega
            sun_dir = np.array(
                [
                    np.cos(sun_lon) * np.cos(params.tilt),
                    np.sin(sun_lon) * np.cos(params.tilt),
                    np.sin(params.tilt),
                ]
            )
            T, wind, w = step_atmosphere_3d(mesh, T, wind, w, params, sun_dir)

        values, label, cmap = get_field_data(T, wind, w, field, layer)
        plot_sphere_scatter(
            ax, mesh, values, f"{label} — Layer {layer} — Frame {frame_num*5}", cmap
        )
        if show_arrows:
            plot_wind_arrows(ax, mesh, wind[layer])
        plot_sun(ax, sun_dir)

        # Print energy periodically
        if frame_num % 10 == 0:
            total_e = np.sum(T**2) + np.sum(wind**2) + np.sum(w**2)
            print(
                f"  Frame {frame_num*5:4d}  Energy={total_e:.1f}  "
                f"|wind|_max={np.linalg.norm(wind, axis=-1).max():.4f}"
            )

    ani = animation.FuncAnimation(
        fig, update, frames=n_frames, interval=100, repeat=False
    )
    plt.show()
    return ani


def animate_remote(field="T", layer=0, n_frames=200, show_arrows=False):
    """Animate by connecting to the running simulation server."""
    from diagnostics import SimulationClient

    client = SimulationClient()
    params_data = client.params()
    n_layers = params_data["n_layers"]
    n_cells = params_data["n_cells"]
    subdivisions = params_data["subdivisions"]

    mesh = IcoMesh(subdivisions=subdivisions)

    fig = plt.figure(figsize=(10, 8))
    ax = fig.add_subplot(111, projection="3d")

    def update(frame_num):
        # Wait for next tick, then get full state
        tick = client.next_tick(timeout=2000)
        if tick is None:
            return
        state = client.full_state()
        if "T" not in state:
            return

        T, wind, w = state["T"], state["wind"], state["w"]
        sun_dir = tick.get("sun_dir", [1, 0, 0])

        values, label, cmap = get_field_data(T, wind, w, field, layer)
        plot_sphere_scatter(
            ax, mesh, values, f"{label} — Layer {layer} — Frame {tick['frame']}", cmap
        )
        if show_arrows:
            plot_wind_arrows(ax, mesh, wind[layer])
        plot_sun(ax, np.array(sun_dir))

    ani = animation.FuncAnimation(
        fig, update, frames=n_frames, interval=200, repeat=False
    )
    plt.show()
    return ani


def main():
    args = sys.argv[1:]

    # Defaults
    layer = 0
    field = "T"
    show_arrows = False
    n_frames = 200
    snapshot_at = None
    remote = False
    show_all = False

    # Parse args
    i = 0
    while i < len(args):
        if args[i] == "--layer" and i + 1 < len(args):
            layer = int(args[i + 1])
            i += 2
        elif args[i] == "--field" and i + 1 < len(args):
            field = args[i + 1]
            i += 2
        elif args[i] == "--arrows":
            show_arrows = True
            i += 1
        elif args[i] == "--frames" and i + 1 < len(args):
            n_frames = int(args[i + 1])
            i += 2
        elif args[i] == "--snapshot" and i + 1 < len(args):
            snapshot_at = int(args[i + 1])
            i += 2
        elif args[i] == "--remote":
            remote = True
            i += 1
        elif args[i] == "--all-layers":
            show_all = True
            i += 1
        else:
            print(f"Unknown arg: {args[i]}")
            print(__doc__)
            sys.exit(1)

    if remote:
        animate_remote(field, layer, n_frames, show_arrows)
        return

    # Standalone mode
    subdivisions = 4
    mesh = IcoMesh(subdivisions=subdivisions)
    params = Params(n_layers=8, tilt=np.radians(23.5))
    print(f"Mesh: {mesh.n_cells} cells, {params.n_layers} layers")

    if snapshot_at is not None:
        # Run to frame N, then show static plot
        print(f"Running {snapshot_at} frames to equilibrium...")
        T = np.zeros((params.n_layers, mesh.n_cells), dtype=np.float64)
        wind = np.zeros((params.n_layers, mesh.n_cells, 3), dtype=np.float64)
        w = np.zeros((params.n_layers, mesh.n_cells), dtype=np.float64)
        sun_lon = 0.0

        for frame in range(snapshot_at):
            sun_lon += params.dt * params.omega
            sun_dir = np.array(
                [
                    np.cos(sun_lon) * np.cos(params.tilt),
                    np.sin(sun_lon) * np.cos(params.tilt),
                    np.sin(params.tilt),
                ]
            )
            T, wind, w = step_atmosphere_3d(mesh, T, wind, w, params, sun_dir)
            if frame % 50 == 0:
                e = np.sum(T**2) + np.sum(wind**2) + np.sum(w**2)
                print(f"  Frame {frame:4d}  Energy={e:.1f}")

        print(f"Final energy: {np.sum(T**2) + np.sum(wind**2) + np.sum(w**2):.1f}")

        if show_all:
            all_layers_snapshot(mesh, T, wind, w, params, field, sun_dir)
        else:
            snapshot(mesh, T, wind, w, params, field, layer, show_arrows, sun_dir)
    else:
        animate_standalone(mesh, params, field, layer, n_frames, show_arrows)


if __name__ == "__main__":
    main()
