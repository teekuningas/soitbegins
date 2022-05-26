import { Elm } from './src/Main.elm'
import * as zip from "@zip.js/zip.js";
import './styles/main.css'


const serverUpdateInterval = parseInt(process.env.SERVER_UPDATE_INTERVAL);
const serverApi = process.env.SERVER_API;
const modelEarth = process.env.MODEL_EARTH;

const flags = {
  "serverUpdateInterval": serverUpdateInterval
}

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
  webWorker.postMessage(obj);

  webWorker.addEventListener("message", function(event) {
    console.log("Main: Got message from the worker!");

    console.log("Splitting data to chunks.. ");
    function splitToChunks(array, parts) {
      let result = [];
      for (let i = parts; i > 0; i--) {
        result.push(array.splice(0, Math.ceil(array.length / i)));
      }
      return result;
    }

    const nParts = 4;
    const chunks = splitToChunks(event.data, nParts);

    console.log("Splitting done.");

    async function sendData() {
      for (let i = 0; i < nParts; i++) {
        const data = {
          "data": chunks[i],
          "totalAmount": nParts,
          "index": i
        }
        console.log("Sending part " + i.toString() + " to elm.");
        app.ports.objReceiver.send(data);

        console.log("Sleeping for a while..");
        await new Promise(r => setTimeout(r, 2000));
      }
    } 
    sendData(); 
  });
});

// To decouple player logic from world logic,
// we update the world parameters here outside of elm realm.
const intervalID = window.setInterval(myCallback, serverUpdateInterval);

var oldTime = Date.now();
function myCallback() {
  let newTime = Date.now(); 
  let elapsed = newTime - oldTime;

  const socket = new WebSocket(serverApi); 
  socket.addEventListener('open', function (event) {
      socket.send('{}');
  });
  socket.addEventListener('message', function (event) {
    const data = JSON.parse(event.data); 
    app.ports.messageReceiver.send(JSON.stringify(data));
    socket.close();
  });
  socket.addEventListener('error', function (event) {
    const data = { "status": "fail" };
    app.ports.messageReceiver.send(JSON.stringify(data));
    socket.close();
  });
}

