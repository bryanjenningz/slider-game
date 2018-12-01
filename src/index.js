import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.playSound.subscribe(soundFile => {
  new Audio(`sounds/${soundFile}`).play();
});

registerServiceWorker();
