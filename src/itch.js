'use strict';

const {Elm} = require('./elm/Main.elm');
const defaultLevel = require('./static/levels/official/001.json');

function runElm() {

  let app = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: {
      jsonLevel: defaultLevel,
      startLevel: 'official/001',
      width: 12,
      height: 12,
      pixelSize: 32,
      additionalViewBorder: 0,
      debug: false
    }
  });

  let trackedKeys = ['ArrowLeft', 'ArrowUp', 'ArrowRight', 'ArrowDown', 'a', 's', 'z', 'x'];
  window.addEventListener('keydown', function(event) {
    if (trackedKeys.includes(event.key)) {
      app.ports.keyDown.send(event.key);
      event.preventDefault();
    }
  }, true);window.addEventListener('keyup', function(event) {
    if (trackedKeys.includes(event.key)) {
      app.ports.keyUp.send(event.key);
      event.preventDefault();
    }
  }, true);
}

runElm();