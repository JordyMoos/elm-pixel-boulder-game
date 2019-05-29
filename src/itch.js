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
        debug: false
    }
  });
}

runElm();