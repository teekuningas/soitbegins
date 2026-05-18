"""
Simulation Microservice
=======================

Runs the 3D multi-layer atmosphere simulation in a dedicated process.
Broadcasts minimal state (wind, T) via ZeroMQ PUB/SUB at a fixed tick rate.
Provides a REQ/REP channel for deep diagnostic queries.

Diagnostic Commands (REQ/REP on :5556):
    ENERGY          → thermal + kinetic energy breakdown
    STATS           → comprehensive per-layer statistics
    PARAMS          → current simulation parameters
    SET key value   → change a parameter at runtime
    RESET           → reset simulation state to zero
    STATE           → full state as binary (multipart: JSON metadata + numpy bytes)
    WIND lat lon alt → interpolated wind at a point (degrees, layer index)
"""

import time
import json
import zmq
import numpy as np
from icosahedron import IcoMesh
from atmosphere_3d import Params, step_atmosphere_3d


def find_nearest_cell(mesh, lat_deg, lon_deg):
    """Find the nearest cell index to a given lat/lon (degrees)."""
    lat = np.radians(lat_deg)
    lon = np.radians(lon_deg)
    target = np.array(
        [np.cos(lat) * np.cos(lon), np.cos(lat) * np.sin(lon), np.sin(lat)]
    )
    dots = mesh.positions @ target
    return int(np.argmax(dots))


def compute_stats(T, wind, w, p):
    """Compute comprehensive statistics for the current state."""
    layers = []
    for k in range(p.n_layers):
        wind_speed = np.linalg.norm(wind[k], axis=1)
        layers.append(
            {
                "layer": k,
                "T_mean": float(np.mean(T[k])),
                "T_max": float(np.max(T[k])),
                "T_min": float(np.min(T[k])),
                "wind_mean": float(np.mean(wind_speed)),
                "wind_max": float(np.max(wind_speed)),
                "w_mean": float(np.mean(w[k])),
                "w_max": float(np.max(np.abs(w[k]))),
                "thermal_energy": float(np.sum(T[k] ** 2)),
                "kinetic_energy": float(np.sum(wind[k] ** 2) + np.sum(w[k] ** 2)),
            }
        )
    return {
        "total_thermal": float(np.sum(T**2)),
        "total_kinetic": float(np.sum(wind**2) + np.sum(w**2)),
        "total_energy": float(np.sum(T**2) + np.sum(wind**2) + np.sum(w**2)),
        "layers": layers,
    }


def handle_diagnostic(msg, rep_socket, mesh, T, wind, w, p, frame, sun_lon):
    """Handle a diagnostic REQ/REP command. Returns updated params if changed."""
    parts = msg.strip().split()
    cmd = parts[0].upper() if parts else ""

    if cmd == "ENERGY":
        thermal = float(np.sum(T**2))
        horiz_ke = float(np.sum(wind**2))
        vert_ke = float(np.sum(w**2))
        rep_socket.send_string(
            json.dumps(
                {
                    "frame": frame,
                    "thermal_energy": thermal,
                    "kinetic_energy_horizontal": horiz_ke,
                    "kinetic_energy_vertical": vert_ke,
                    "total_energy": thermal + horiz_ke + vert_ke,
                }
            )
        )

    elif cmd == "STATS":
        stats = compute_stats(T, wind, w, p)
        stats["frame"] = frame
        stats["sun_lon"] = float(sun_lon)
        rep_socket.send_string(json.dumps(stats))

    elif cmd == "PARAMS":
        rep_socket.send_string(
            json.dumps(
                {
                    "frame": frame,
                    "dt": p.dt,
                    "solar": p.solar,
                    "cooling": p.cooling,
                    "c_sq": p.c_sq,
                    "drag": p.drag,
                    "omega": p.omega,
                    "g": p.g,
                    "n_layers": p.n_layers,
                    "tilt": p.tilt,
                    "n_cells": mesh.n_cells,
                    "subdivisions": mesh.subdivisions,
                }
            )
        )

    elif cmd == "SET" and len(parts) == 3:
        key, value = parts[1], parts[2]
        settable = {"solar", "cooling", "c_sq", "drag", "omega", "g", "tilt", "dt"}
        if key in settable:
            try:
                setattr(p, key, float(value))
                rep_socket.send_string(
                    json.dumps({"ok": True, "param": key, "value": getattr(p, key)})
                )
            except ValueError:
                rep_socket.send_string(json.dumps({"error": f"Invalid value: {value}"}))
        else:
            rep_socket.send_string(
                json.dumps(
                    {
                        "error": f"Unknown/unsettable param: {key}",
                        "settable": list(settable),
                    }
                )
            )

    elif cmd == "RESET":
        T[:] = 0.0
        wind[:] = 0.0
        w[:] = 0.0
        rep_socket.send_string(
            json.dumps({"ok": True, "message": "State reset to zero"})
        )

    elif cmd == "STATE":
        metadata = {
            "frame": frame,
            "n_layers": p.n_layers,
            "n_cells": mesh.n_cells,
            "T_shape": list(T.shape),
            "wind_shape": list(wind.shape),
            "w_shape": list(w.shape),
            "dtype": "float64",
            "sun_lon": float(sun_lon),
        }
        rep_socket.send_multipart(
            [json.dumps(metadata).encode(), T.tobytes(), wind.tobytes(), w.tobytes()]
        )

    elif cmd == "WIND" and len(parts) == 4:
        try:
            lat, lon, alt = float(parts[1]), float(parts[2]), int(parts[3])
            cell = find_nearest_cell(mesh, lat, lon)
            alt = max(0, min(p.n_layers - 1, alt))
            rep_socket.send_string(
                json.dumps(
                    {
                        "lat": lat,
                        "lon": lon,
                        "layer": alt,
                        "cell": cell,
                        "wind": wind[alt, cell].tolist(),
                        "wind_speed": float(np.linalg.norm(wind[alt, cell])),
                        "w": float(w[alt, cell]),
                        "T": float(T[alt, cell]),
                    }
                )
            )
        except (ValueError, IndexError) as e:
            rep_socket.send_string(json.dumps({"error": str(e)}))

    else:
        rep_socket.send_string(
            json.dumps(
                {
                    "error": f"Unknown command: {msg}",
                    "commands": [
                        "ENERGY",
                        "STATS",
                        "PARAMS",
                        "SET key value",
                        "RESET",
                        "STATE",
                        "WIND lat lon alt",
                    ],
                }
            )
        )


def run_server():
    context = zmq.Context()

    pub_socket = context.socket(zmq.PUB)
    pub_socket.bind("tcp://0.0.0.0:5555")

    rep_socket = context.socket(zmq.REP)
    rep_socket.bind("tcp://0.0.0.0:5556")

    print("Simulation Microservice starting...")
    print("PUB bound to tcp://0.0.0.0:5555 (State Broadcast)")
    print("REP bound to tcp://0.0.0.0:5556 (Diagnostics Channel)")

    subdivisions = 4
    mesh = IcoMesh(subdivisions=subdivisions)
    p = Params(n_layers=8, tilt=np.radians(23.5))

    T = np.zeros((p.n_layers, mesh.n_cells), dtype=np.float64)
    wind = np.zeros((p.n_layers, mesh.n_cells, 3), dtype=np.float64)
    w = np.zeros((p.n_layers, mesh.n_cells), dtype=np.float64)

    sun_lon = 0.0
    frame = 0

    # 2 Hz: physics steps 2×/sec. dt=π/120 per step → day = 2π/(omega×dt×2) ≈ 300s = 5 min.
    # Same equilibrium physics as 10 Hz — only real-time pacing changes.
    tick_rate = 2
    tick_duration = 1.0 / tick_rate

    print(
        f"Mesh: {mesh.n_cells} cells, {p.n_layers} layers. Tick rate: {tick_rate} Hz."
    )
    print(
        f"Params: solar={p.solar}, cooling={p.cooling}, c_sq={p.c_sq}, drag={p.drag}, omega={p.omega}, g={p.g}"
    )
    print("Diagnostic commands: ENERGY, STATS, PARAMS, SET, RESET, STATE, WIND")

    while True:
        loop_start = time.time()

        # 1. Advance sun and step physics
        sun_lon += p.dt * p.omega
        sun_dir = np.array(
            [
                np.cos(sun_lon) * np.cos(p.tilt),
                np.sin(sun_lon) * np.cos(p.tilt),
                np.sin(p.tilt),
            ]
        )

        T, wind, w = step_atmosphere_3d(mesh, T, wind, w, p, sun_dir)

        # 2. Handle diagnostics (non-blocking, process all pending)
        while True:
            try:
                msg = rep_socket.recv_string(flags=zmq.NOBLOCK)
                handle_diagnostic(msg, rep_socket, mesh, T, wind, w, p, frame, sun_lon)
            except zmq.Again:
                break

        # 3. Broadcast tick
        pub_socket.send_json(
            {
                "type": "TICK",
                "frame": frame,
                "sun_lon": float(sun_lon),
                "sim_time": float(frame * p.dt),
                "sun_dir": sun_dir.tolist(),
            }
        )

        frame += 1

        # 4. Enforce tick rate
        elapsed = time.time() - loop_start
        sleep_time = tick_duration - elapsed
        if sleep_time > 0:
            time.sleep(sleep_time)


if __name__ == "__main__":
    run_server()
