import { Elm } from './src/Main.elm'
import './styles/main.css'

// Start the Elm application.
var app = Elm.Main.init({
  node: document.getElementById('root')
});

// To decouple player logic from world logic,
// we update the world parameters here outside of elm realm.
var intervalID = window.setInterval(myCallback, 1000);

var oldTime = Date.now();
function myCallback() {
  let newTime = Date.now(); 
  let elapsed = newTime - oldTime;

  // var socket = new WebSocket('ws://localhost:8765'); 
  var socket = new WebSocket('wss://soitbegins.teekuningas.net/world'); 
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
