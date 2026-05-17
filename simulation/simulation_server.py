"""
Simulation Microservice
=======================

Runs the 3D multi-layer atmosphere simulation in a dedicated process.
Broadcasts minimal state (wind, T) via ZeroMQ PUB/SUB at a fixed tick rate.
Provides a REQ/REP channel for deep diagnostic queries.
"""

import time
import json
import zmq
import numpy as np
from icosahedron import IcoMesh
from atmosphere_3d import Params, step_atmosphere_3d


def run_server():
    context = zmq.Context()

    # PUB socket for state broadcast (Game Server and Visualizers subscribe to this)
    pub_socket = context.socket(zmq.PUB)
    pub_socket.bind("tcp://0.0.0.0:5555")

    # REP socket for diagnostics / sensitivity analysis commands
    rep_socket = context.socket(zmq.REP)
    rep_socket.bind("tcp://0.0.0.0:5556")

    print("Simulation Microservice starting...")
    print("PUB bound to tcp://0.0.0.0:5555 (State Broadcast)")
    print("REP bound to tcp://0.0.0.0:5556 (Diagnostics Channel)")

    # Initialize physics mesh and state
    subdivisions = 4  # Level 4 for dev. Move to 5 for production.
    mesh = IcoMesh(subdivisions=subdivisions)
    p = Params(n_layers=8, tilt=np.radians(23.5))

    T = np.zeros((p.n_layers, mesh.n_cells), dtype=np.float64)
    wind = np.zeros((p.n_layers, mesh.n_cells, 3), dtype=np.float64)
    w = np.zeros((p.n_layers, mesh.n_cells), dtype=np.float64)

    sun_lon = 0.0
    frame = 0

    # Timing
    tick_rate = 10  # 10 ticks per second
    tick_duration = 1.0 / tick_rate

    print(f"Mesh: {mesh.n_cells} cells. Tick rate: {tick_rate} Hz.")

    while True:
        loop_start = time.time()

        # 1. Coordinate Math & Physics Step
        sun_lon += p.dt * p.omega
        sun_dir = np.array(
            [
                np.cos(sun_lon) * np.cos(p.tilt),
                np.sin(sun_lon) * np.cos(p.tilt),
                np.sin(p.tilt),
            ]
        )

        T, wind, w = step_atmosphere_3d(mesh, T, wind, w, p, sun_dir)

        # 2. Diagnostics REP/REQ Channel (Non-blocking)
        try:
            msg = rep_socket.recv_string(flags=zmq.NOBLOCK)
            if msg == "ENERGY":
                thermal = np.sum(T**2)
                horiz_ke = np.sum(wind**2)
                response = {
                    "frame": frame,
                    "thermal_energy": float(thermal),
                    "kinetic_energy": float(horiz_ke),
                }
                rep_socket.send_string(json.dumps(response))
            elif msg == "STATE":
                # In the future, serialize and send full exact NumPy arrays via msgpack/savez
                rep_socket.send_string(
                    json.dumps(
                        {"error": "Full state binary export not yet implemented."}
                    )
                )
            else:
                rep_socket.send_string(json.dumps({"error": "Unknown command"}))
        except zmq.Again:
            pass  # No diagnostic requests this tick

        # 3. Game Server PUB Broadcast
        # Broadcasting a tiny subset of data or a quantized version to keep throughput high.
        # For now, we will send a JSON payload with basic metrics to prove the channel works.
        # Phase C will transition this to a dense binary packing (int8).
        pub_socket.send_json(
            {
                "type": "TICK",
                "frame": frame,
                "sun_dir": sun_dir.tolist(),
                # "wind": ... (Binary payload will go here)
            }
        )

        frame += 1

        # 4. Enforce Tick Rate
        elapsed = time.time() - loop_start
        sleep_time = tick_duration - elapsed
        if sleep_time > 0:
            time.sleep(sleep_time)


if __name__ == "__main__":
    run_server()
