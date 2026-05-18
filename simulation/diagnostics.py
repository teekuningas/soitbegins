"""
Simulation Diagnostic Client
=============================

Connects to the running simulation microservice via ZeroMQ.
Provides both a Python API and a CLI for monitoring, querying,
and exporting simulation data.

Usage:
    python diagnostics.py                    # Interactive REPL
    python diagnostics.py energy             # One-shot energy query
    python diagnostics.py stats              # Full statistics
    python diagnostics.py params             # Current parameters
    python diagnostics.py monitor [frames]   # Monitor energy over N ticks
    python diagnostics.py export [frames]    # Export N frames to CSV
    python diagnostics.py wind 60 25 3       # Wind at lat=60, lon=25, layer=3
    python diagnostics.py set solar 0.25     # Change parameter at runtime
    python diagnostics.py reset              # Reset state to zero
"""

import sys
import time
import json
import csv
import zmq
import numpy as np


class SimulationClient:
    """ZMQ client for the simulation microservice."""

    def __init__(self, host="localhost", pub_port=5555, rep_port=5556):
        self.context = zmq.Context()
        self.host = host
        self.pub_port = pub_port
        self.rep_port = rep_port
        self._sub = None
        self._req = None

    @property
    def sub(self):
        if self._sub is None:
            self._sub = self.context.socket(zmq.SUB)
            self._sub.connect(f"tcp://{self.host}:{self.pub_port}")
            self._sub.setsockopt_string(zmq.SUBSCRIBE, "")
        return self._sub

    @property
    def req(self):
        if self._req is None:
            self._req = self.context.socket(zmq.REQ)
            self._req.connect(f"tcp://{self.host}:{self.rep_port}")
        return self._req

    def query(self, command):
        """Send a diagnostic command, return parsed JSON response."""
        self.req.send_string(command)
        response = self.req.recv_multipart()
        if len(response) == 1:
            return json.loads(response[0])
        else:
            # Multipart: first frame is JSON metadata, rest are binary
            metadata = json.loads(response[0])
            metadata["_binary_parts"] = response[1:]
            return metadata

    def energy(self):
        return self.query("ENERGY")

    def stats(self):
        return self.query("STATS")

    def params(self):
        return self.query("PARAMS")

    def set_param(self, key, value):
        return self.query(f"SET {key} {value}")

    def reset(self):
        return self.query("RESET")

    def wind_at(self, lat, lon, layer):
        return self.query(f"WIND {lat} {lon} {layer}")

    def full_state(self):
        """Request full state as numpy arrays."""
        result = self.query("STATE")
        if "_binary_parts" in result:
            n_layers = result["n_layers"]
            n_cells = result["n_cells"]
            parts = result["_binary_parts"]
            T = np.frombuffer(parts[0], dtype=np.float64).reshape(n_layers, n_cells)
            wind = np.frombuffer(parts[1], dtype=np.float64).reshape(
                n_layers, n_cells, 3
            )
            w = np.frombuffer(parts[2], dtype=np.float64).reshape(n_layers, n_cells)
            return {"metadata": result, "T": T, "wind": wind, "w": w}
        return result

    def next_tick(self, timeout=5000):
        """Wait for next PUB tick. Returns parsed JSON or None on timeout."""
        if self.sub.poll(timeout):
            return self.sub.recv_json()
        return None

    def monitor(self, n_frames=100, callback=None):
        """Monitor energy over N tick cycles, calling callback(frame_data) each tick."""
        history = []
        for i in range(n_frames):
            tick = self.next_tick()
            if tick is None:
                print(f"Timeout waiting for tick at frame {i}")
                break
            data = self.energy()
            data["sun_dir"] = tick.get("sun_dir")
            history.append(data)
            if callback:
                callback(data)
        return history

    def export_csv(self, n_frames=100, filename="simulation_data.csv"):
        """Export N frames of energy data to CSV."""
        history = []

        def collector(data):
            history.append(data)

        self.monitor(n_frames, callback=collector)

        if not history:
            print("No data collected.")
            return

        with open(filename, "w", newline="") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=[k for k in history[0].keys() if k != "sun_dir"],
            )
            writer.writeheader()
            for row in history:
                row_out = {k: v for k, v in row.items() if k != "sun_dir"}
                writer.writerow(row_out)
        print(f"Exported {len(history)} frames to {filename}")

    def close(self):
        if self._sub:
            self._sub.close()
        if self._req:
            self._req.close()
        self.context.term()


def format_json(data):
    """Pretty-print JSON data, omitting binary parts."""
    filtered = {k: v for k, v in data.items() if k != "_binary_parts"}
    return json.dumps(filtered, indent=2)


def repl(client):
    """Interactive REPL for the diagnostic client."""
    print("Simulation Diagnostic Client — REPL")
    print(
        "Commands: energy, stats, params, set <key> <val>, reset, wind <lat> <lon> <layer>"
    )
    print("          monitor [N], export [N] [file], state, quit")
    print()

    while True:
        try:
            line = input("sim> ").strip()
        except (EOFError, KeyboardInterrupt):
            print()
            break

        if not line:
            continue

        parts = line.split()
        cmd = parts[0].lower()

        try:
            if cmd == "quit" or cmd == "exit":
                break
            elif cmd == "energy":
                print(format_json(client.energy()))
            elif cmd == "stats":
                print(format_json(client.stats()))
            elif cmd == "params":
                print(format_json(client.params()))
            elif cmd == "set" and len(parts) == 3:
                print(format_json(client.set_param(parts[1], parts[2])))
            elif cmd == "reset":
                print(format_json(client.reset()))
            elif cmd == "wind" and len(parts) == 4:
                print(format_json(client.wind_at(parts[1], parts[2], parts[3])))
            elif cmd == "monitor":
                n = int(parts[1]) if len(parts) > 1 else 20
                print(f"Monitoring {n} frames...")
                history = client.monitor(
                    n,
                    callback=lambda d: print(
                        f"  frame {d['frame']:5d}  E={d['total_energy']:.1f}  "
                        f"T={d['thermal_energy']:.1f}  KE_h={d['kinetic_energy_horizontal']:.1f}  "
                        f"KE_v={d['kinetic_energy_vertical']:.1f}"
                    ),
                )
            elif cmd == "export":
                n = int(parts[1]) if len(parts) > 1 else 100
                fname = parts[2] if len(parts) > 2 else "simulation_data.csv"
                client.export_csv(n, fname)
            elif cmd == "state":
                result = client.full_state()
                if "T" in result:
                    T, wind, w = result["T"], result["wind"], result["w"]
                    print(f"State received: T{T.shape}, wind{wind.shape}, w{w.shape}")
                    print(f"  T range: [{T.min():.4f}, {T.max():.4f}]")
                    print(f"  |wind| max: {np.linalg.norm(wind, axis=-1).max():.4f}")
                    print(f"  |w| max: {np.abs(w).max():.4f}")
                else:
                    print(format_json(result))
            else:
                # Pass through as raw command
                print(format_json(client.query(line)))
        except Exception as e:
            print(f"Error: {e}")


def main():
    host = "localhost"
    args = sys.argv[1:]

    client = SimulationClient(host=host)

    try:
        if not args:
            repl(client)
        elif args[0] == "energy":
            print(format_json(client.energy()))
        elif args[0] == "stats":
            print(format_json(client.stats()))
        elif args[0] == "params":
            print(format_json(client.params()))
        elif args[0] == "set" and len(args) == 3:
            print(format_json(client.set_param(args[1], args[2])))
        elif args[0] == "reset":
            print(format_json(client.reset()))
        elif args[0] == "wind" and len(args) == 4:
            print(format_json(client.wind_at(args[1], args[2], args[3])))
        elif args[0] == "monitor":
            n = int(args[1]) if len(args) > 1 else 50
            print(f"Monitoring {n} frames (Ctrl+C to stop)...")
            client.monitor(
                n,
                callback=lambda d: print(
                    f"frame {d['frame']:5d}  E={d['total_energy']:.1f}  "
                    f"T={d['thermal_energy']:.1f}  KE_h={d['kinetic_energy_horizontal']:.1f}  "
                    f"KE_v={d['kinetic_energy_vertical']:.1f}"
                ),
            )
        elif args[0] == "export":
            n = int(args[1]) if len(args) > 1 else 100
            fname = args[2] if len(args) > 2 else "simulation_data.csv"
            client.export_csv(n, fname)
        else:
            # Raw command passthrough
            print(format_json(client.query(" ".join(args))))
    except KeyboardInterrupt:
        print()
    finally:
        client.close()


if __name__ == "__main__":
    main()
