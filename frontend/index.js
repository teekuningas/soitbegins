import { Elm } from './src/Main.elm'
import './styles/main.css'

const runtimeServerUpdateInterval = "%%RUNTIME_SERVER_UPDATE_INTERVAL%%";
const runtimeServerApi = "%%RUNTIME_SERVER_API%%";

const serverUpdateInterval = parseInt(!runtimeServerUpdateInterval.includes("RUNTIME_SERVER_UPDATE_INTERVAL") ? runtimeServerUpdateInterval : "1000");
const serverApi = !runtimeServerApi.includes("RUNTIME_SERVER_API") ? runtimeServerApi : "ws://localhost:8765";

const flags = {
  "serverUpdateInterval": serverUpdateInterval
};

// Start the Elm application.
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});

// Generate earth mesh from cubemap textures using web worker
const meshWorker =
  new Worker(
    new URL("./js/generateMesh.js", import.meta.url),
    {type: 'module'}
  );

const nParts = 4;
const subdivisions = 5; // 20,480 triangles — good balance of detail and performance

meshWorker.postMessage({
  cubemapBaseUrl: "",  // Static files served at root by parcel-reporter-static-files-copy
  subdivisions: subdivisions,
  nParts: nParts
});

meshWorker.addEventListener("message", function(event) {
  console.log("Main: Got mesh from worker!");

  async function sendData() {
    for (let i = 0; i < nParts; i++) {
      const data = {
        "data": event.data[i],
        "totalAmount": nParts,
        "index": i
      }
      console.log("Sending part " + i.toString() + " to elm.");
      app.ports.objReceiver.send(data);

      console.log("Sleeping for a while..");
      await new Promise(r => setTimeout(r, 1000));
    }
  }
  sendData();
});

// To decouple player logic from world logic,
// we update the world parameters here outside of elm realm.

const socket = new WebSocket(serverApi);

socket.addEventListener('message', function (event) {
  const data = JSON.parse(event.data);
  app.ports.messageReceiver.send(JSON.stringify(data));
});

socket.addEventListener('error', function (event) {
  const data = { "status": "fail" };
  app.ports.messageReceiver.send(JSON.stringify(data));
  socket.close();
});

function myCallback() {
  if (socket.readyState !== WebSocket.CLOSED) {
    socket.send('{}');
  }
}

const intervalID = window.setInterval(myCallback, serverUpdateInterval);

