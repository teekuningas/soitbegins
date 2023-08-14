import { Elm } from './src/Main.elm'
import * as zip from "@zip.js/zip.js";
import './styles/main.css'

const runtimeServerUpdateInterval = "%%RUNTIME_SERVER_UPDATE_INTERVAL%%";
const runtimeServerApi = "%%RUNTIME_SERVER_API%%";
const runtimeModelEarth = "%%RUNTIME_MODEL_EARTH%%";

const serverUpdateInterval = parseInt(!runtimeServerUpdateInterval.includes("RUNTIME_SERVER_UPDATE_INTERVAL") ? runtimeServerUpdateInterval : "1000");
const serverApi = !runtimeServerApi.includes("RUNTIME_SERVER_API") ? runtimeServerApi : "ws://localhost:8765";
const modelEarth = !runtimeModelEarth.includes("RUNTIME_MODEL_EARTH") ? runtimeModelEarth : "http://localhost:1234/earth.zip";

const flags = {
  "serverUpdateInterval": serverUpdateInterval
};

// Start the Elm application.
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});

// Download and unzip earth heightmap
async function unzipObjFile() {
  const reader = new zip.ZipReader(
    new zip.HttpReader(modelEarth));

  const entries = await reader.getEntries();

  var text;
  if (entries.length) {
    text = await entries[0].getData(
      new zip.TextWriter()
    );
    await reader.close();
    return text;
  }
}
const objFile = unzipObjFile();

console.log("Unzipping done")

const webWorker = 
  new Worker(
    new URL("./js/parseObj.js", import.meta.url),
    {type: 'module'}
  );


objFile.then(obj => {

  const nParts = 4;

  webWorker.postMessage({"data": obj, "nParts": nParts});

  webWorker.addEventListener("message", function(event) {

    console.log("Main: Got message from the worker!");

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

