import asyncio
import json
import time
import websockets
import datetime

from pprint import pprint

time_at_beginning = datetime.datetime.now().timestamp() * 1000

state = {
    'elapsed': 0,
    'earth': {'locationX': 5,
              'locationY': 0,
              'locationZ': 0,
              'rotationTheta': 0}
}

async def update():
    global counter
    while True:
        await asyncio.sleep(0.1)

        # update state
        state['elapsed'] = (datetime.datetime.now().timestamp() * 1000 - 
                            time_at_beginning)

        state['earth']['rotationTheta'] = state['elapsed'] / 2000


def process_incoming(incoming):
    return state.copy()

async def hello(websocket, path):

    incoming = await websocket.recv()

    print("Received: ")
    pprint(incoming)

    msgdict = process_incoming(incoming)

    message = json.dumps(msgdict)

    await websocket.send(message)

    print("Sending: ")
    pprint(message)


if __name__ == '__main__':

    start_server = websockets.serve(hello, 'localhost', 8765)
    event_loop = asyncio.get_event_loop()
    event_loop.run_until_complete(start_server)
    event_loop.create_task(update())
    event_loop.run_forever()
