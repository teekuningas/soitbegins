"""Communication Server

Bridges the simulation (ZMQ PUB) to frontend clients (WebSocket).
Subscribes to simulation ticks and caches the latest state.
When the frontend polls, responds with current earth rotation parameters.

Falls back to wall-clock rotation if simulation is unavailable.
"""

import asyncio
import json
import os
import sys
import time

import websockets
import zmq
import zmq.asyncio


# Simulation ZMQ address (container name or localhost for dev)
SIM_ZMQ_ADDR = os.environ.get("SIM_ZMQ_ADDR", "tcp://localhost:5555")

# WebSocket config
WS_ADDRESS = os.environ.get("SERVER_ADDRESS", "0.0.0.0")
WS_PORT = int(os.environ.get("SERVER_PORT", "8765"))

# Orbital period ratio (one orbit per N rotations — gameplay choice)
ORBIT_RATIO = 10


class SimulationState:
    """Holds the latest simulation state received via ZMQ."""

    def __init__(self):
        self.sun_lon = 0.0
        self.sim_time = 0.0
        self.last_update = 0.0
        self.connected = False
        # Fallback: wall-clock rotation when simulation unavailable
        self.start_time = time.time()

    def get_earth(self):
        """Return earth rotation state for the frontend."""
        if self.connected and (time.time() - self.last_update) < 5.0:
            rotation_around_axis = self.sun_lon
            rotation_around_sun = self.sun_lon / ORBIT_RATIO
        else:
            # Fallback: same rates as old server (~60s per rotation)
            elapsed_s = time.time() - self.start_time
            rotation_around_axis = elapsed_s * 0.1047  # ~60s per rotation
            rotation_around_sun = elapsed_s * 0.01047

        return {
            "rotationAroundAxis": rotation_around_axis,
            "rotationAroundSun": rotation_around_sun,
        }


sim_state = SimulationState()


async def zmq_subscriber():
    """Subscribe to simulation ticks via ZMQ PUB/SUB."""
    ctx = zmq.asyncio.Context()
    sub = ctx.socket(zmq.SUB)
    sub.setsockopt_string(zmq.SUBSCRIBE, "")
    sub.setsockopt(zmq.RCVTIMEO, 5000)

    print(f"Connecting to simulation at {SIM_ZMQ_ADDR}...", flush=True)
    sub.connect(SIM_ZMQ_ADDR)

    while True:
        try:
            msg = await sub.recv_json()
            if msg.get("type") == "TICK":
                sim_state.sun_lon = msg["sun_lon"]
                sim_state.sim_time = msg.get("sim_time", 0.0)
                sim_state.last_update = time.time()
                if not sim_state.connected:
                    sim_state.connected = True
                    print("Simulation connected.", flush=True)
        except zmq.Again:
            if sim_state.connected:
                print("Simulation connection lost, falling back.", flush=True)
                sim_state.connected = False
        except Exception as e:
            print(f"ZMQ error: {e}", flush=True)
            await asyncio.sleep(1)


async def websocket_handler(websocket):
    """Handle frontend WebSocket connections."""
    while True:
        incoming = await websocket.recv()

        state = {
            "elapsed": sim_state.sim_time * 1000,
            "earth": sim_state.get_earth(),
        }

        await websocket.send(json.dumps(state))


async def main():
    print(f"Server starting on {WS_ADDRESS}:{WS_PORT}", flush=True)

    # Start ZMQ subscriber as background task
    asyncio.create_task(zmq_subscriber())

    # Start WebSocket server
    async with websockets.serve(websocket_handler, WS_ADDRESS, WS_PORT):
        print(f"WebSocket ready on ws://{WS_ADDRESS}:{WS_PORT}", flush=True)
        await asyncio.Future()  # Run forever


if __name__ == "__main__":
    asyncio.run(main())
