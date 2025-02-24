import { Elm } from "./Main.elm";

const elmElement = document.getElementById("elm");
const app = Elm.Main.init({
  node: elmElement,
});

// Listen for messages from Elm
app.ports.sendMessage.subscribe((message) => {
  console.log("Message from Elm:", message);
});

// Send a message to Elm
setTimeout(() => {
  app.ports.receiveMessage.send("Hello from JavaScript!");
}, 3000);
