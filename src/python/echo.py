
import asyncio
import websockets
import os

async def echo(websocket, path):
    async for message in websocket:
        os.system('cls' if os.name == 'nt' else 'clear')
        print(message)
        await websocket.send('Done')

asyncio.get_event_loop().run_until_complete(websockets.serve(echo, 'localhost', 8765))
asyncio.get_event_loop().run_forever()