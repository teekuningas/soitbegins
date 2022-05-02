import { Elm } from './src/Main.elm'
import './styles/main.css'


const serverUpdateInterval = parseInt(process.env.SERVER_UPDATE_INTERVAL);
const serverApi = process.env.SERVER_API;
const modelEarth = process.env.MODEL_EARTH;

var flags = {
  "serverUpdateInterval": serverUpdateInterval,
  "modelEarth": modelEarth
}

// Start the Elm application.
var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
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

