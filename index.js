import { Elm } from "./src/Main.elm";

const App = Elm.Main.init({
  node: document.body,
});

App.ports.requestPointerLock.subscribe(() => {
  document.querySelector("canvas").requestPointerLock();
});
