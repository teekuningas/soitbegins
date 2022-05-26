import { Elm } from './src/Main.elm'
import * as zip from "@zip.js/zip.js";
import './styles/main.css'


const serverUpdateInterval = parseInt(process.env.SERVER_UPDATE_INTERVAL);
const serverApi = process.env.SERVER_API;
const modelEarth = process.env.MODEL_EARTH;

var flags = {
  "serverUpdateInterval": serverUpdateInterval
}

// Start the Elm application.
var app = Elm.Main.init({
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

var webWorker = 
  new Worker(
    new URL("./js/parseObj.js", import.meta.url),
    {type: 'module'}
  );


objFile.then(obj => {
  webWorker.postMessage(obj);

  webWorker.addEventListener("message", function(event) {
    console.log("Got the message from worker");
    app.ports.objReceiver.send(event.data);
  });
});

// To decouple player logic from world logic,
// we update the world parameters here outside of elm realm.
var intervalID = window.setInterval(myCallback, serverUpdateInterval);

var oldTime = Date.now();
function myCallback() {
  let newTime = Date.now(); 
  let elapsed = newTime - oldTime;

  var socket = new WebSocket(serverApi); 
  socket.addEventListener('open', function (event) {
      socket.send('{}');
  });
  socket.addEventListener('message', function (event) {
    var data = JSON.parse(event.data); 
    app.ports.messageReceiver.send(JSON.stringify(data));
    socket.close();
  });
  socket.addEventListener('error', function (event) {
    var data = { "status": "fail" };
    app.ports.messageReceiver.send(JSON.stringify(data));
    socket.close();
  });
}

