import { Elm } from "./Main.elm";

const GAME_KEY = "wolfadex__elm-text-adventure__example__game";

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: JSON.parse(localStorage.getItem(GAME_KEY) || "{}")
});

app.ports.saveGame.subscribe(function(gameData) {
  localStorage.setItem(GAME_KEY, JSON.stringify(gameData));
});