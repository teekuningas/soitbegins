import asyncio
import datetime
import json
import os
import sys
import time
import websockets

from pprint import pprint


# time at the beginning
time_at_beginning = datetime.datetime.now().timestamp() * 1000


# holds the game state
state = {"elapsed": 0, "earth": {"rotationAroundSun": 0, "rotationAroundAxis": 0}}


# updates the game state periodically
async def update():
    while True:
        await asyncio.sleep(0.1)

        # update time
        state["elapsed"] = (
            datetime.datetime.now().timestamp() * 1000 - time_at_beginning
        )

        # update earth rotation
        state["earth"]["rotationAroundAxis"] = state["elapsed"] / 10000
        state["earth"]["rotationAroundSun"] = state["elapsed"] / 100000


# update game state with incoming messages
def process_incoming(incoming):
    # at the moment, just ignore the incoming message
    return state


# run exactly once for each incoming socket connection
async def socket_fun(websocket, path):
    while True:
        # receive a message
        incoming = await websocket.recv()

        print("Received: ")
        pprint(incoming)

        # update the state with incoming messages
        msgdict = process_incoming(incoming)

        # convert to json
        message = json.dumps(msgdict)

        # send the state to the client
        await websocket.send(message)

        print("Sending: ")
        pprint(message)

        sys.stdout.flush()


# when the script is run
if __name__ == "__main__":
    address = (
        os.environ["SERVER_ADDRESS"] if os.environ.get("SERVER_ADDRESS") else "0.0.0.0"
    )

    port = int(os.environ["SERVER_PORT"]) if os.environ.get("SERVER_PORT") else 8765

    # get event loop
    event_loop = asyncio.get_event_loop()

    print(f"Serving at {address}:{port}..", flush=True)

    # start the server
    start_server = websockets.serve(socket_fun, address, port)
    event_loop.run_until_complete(start_server)

    # start a task that updates the game state
    event_loop.create_task(update())

    # do not quit..
    event_loop.run_forever()
