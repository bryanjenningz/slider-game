import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: localStorage.getItem('saved-data')
});

app.ports.saveData.subscribe(savedData => {
  localStorage.setItem('saved-data', JSON.stringify(savedData));
});

app.ports.playSound.subscribe(soundFile => {
  new Audio(`sounds/${soundFile}`).play();
});

registerServiceWorker();
