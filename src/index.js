'use strict';

const {Elm} = require('./elm/Main.elm');
const cacheKey = 'level-cache-1';

const defaultLevel = require('./static/levels/test/pixel.json');
const levels = {
  'level-pixel': defaultLevel,
  'level-nes-small': require('./static/levels/test/nes-small.json'),
  'level-nes': require('./static/levels/test/nes.json'),
  'level-image': require('./static/levels/test/images.json'),
  'level-pacman': require('./static/levels/test/pacman.json'),
  'level-tank': require('./static/levels/test/tank.json'),
  'level-game-of-life': require('./static/levels/test/game-of-life.json')
};


const urlParams = new URLSearchParams(window.location.search);
const startLevel = urlParams.get('startLevel') || null;
const hideDebug = !! urlParams.get('hideDebug');
const editorMode = urlParams.get('editorMode') === 'easy' ? 'easy' : 'advanced';
const notEditorMode = getOtherMode(editorMode);

function getOtherMode(mode) {
  return mode === 'easy' ? 'advanced' : 'easy';
}


['easy', 'advanced'].forEach(function(mode) {
  document.getElementById(mode + '-editor-option').addEventListener('change', function (data) {
    let newMode = data.target.value;
    let newOtherMode = getOtherMode(newMode);

    document.getElementById(newMode + '-editor').style.display = '';
    document.getElementById( newOtherMode + '-editor').style.display = 'none';

    
  });
});

document.getElementById(editorMode + '-editor-option').checked = 'checked';
document.getElementById(editorMode + '-editor').style.display = '';
document.getElementById(notEditorMode + '-editor').style.display = 'none';

if (hideDebug) {
  document.getElementById('edit-level-container').style.display = 'none';
}

function runElm() {
  document.getElementById('editor-container').style.display = 'none';
  document.getElementById('game-container').style.display = '';

  try {

    let app = Elm.Main.init({
      node: document.getElementById('elm'),
      flags: {
        jsonLevel: JSON.parse(document.getElementById('advanced-textarea-level').value),
        startLevel: startLevel,
        width: urlParams.get('width')|0 || 12,
        height: urlParams.get('height')|0 || 12,
        pixelSize: urlParams.get('pixelSize')|0 || 32,
	      debug: ! hideDebug
      }
    });

    ['ArrowLeft', 'ArrowUp', 'ArrowRight', 'ArrowDown', 'a', 's', 'z', 'x'].forEach(function (buttonName) {
      ['mousedown', 'touchstart'].forEach(function (eventName) {
        document.getElementById('button-' + buttonName).addEventListener(eventName, function () {
          app.ports.keyDown.send(buttonName);
        });
      });
      ['click', 'touchend'].forEach(function (eventName) {
        document.getElementById('button-' + buttonName).addEventListener(eventName, function () {
          app.ports.keyUp.send(buttonName);
        });
      });
    });

  } catch (e) {
    console.dir(e);
    document.getElementById('elm').innerText = 'Error: ' + e.message;
  }
}

document.getElementById('advanced-textarea-level').value =
  localStorage.getItem(cacheKey) || JSON.stringify(defaultLevel, null, 2);

Object.keys(levels).forEach(function(id) {
  document.getElementById(id)
    .addEventListener('click', function () {
      document.getElementById('advanced-textarea-level').value = JSON.stringify(levels[id], null, 2);
    });
});

document.getElementById('submit-level')
  .addEventListener('click', function () {
      localStorage.setItem(cacheKey, document.getElementById('advanced-textarea-level').value);
      runElm();
    }
  );

document.getElementById('edit-level')
  .addEventListener('click', function () {
      location.reload();
    }
  );

if (startLevel !== null) {
  runElm();
}
