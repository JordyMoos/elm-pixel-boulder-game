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
const startLevel = urlParams.get('startLevel');

function runElm(startLevel) {
  document.getElementById('editor-container').style.display = 'none';
  document.getElementById('game-container').style.display = '';

  try {

    Elm.Main.init({
      node: document.getElementById('elm'),
      flags: {
        jsonLevel: JSON.parse(document.getElementById('textarea-level').value),
        startLevel: startLevel
      }
    });
  } catch (e) {
    console.dir(e);
    document.getElementById('elm').innerText = 'Error: ' + e.message;
  }
}

document.getElementById('textarea-level').value =
  localStorage.getItem(cacheKey) || JSON.stringify(defaultLevel, null, 2);

Object.keys(levels).forEach(function(id) {
  document.getElementById(id)
    .addEventListener('click', function () {
      document.getElementById('textarea-level').value = JSON.stringify(levels[id], null, 2);
    });
});

document.getElementById('submit-level')
  .addEventListener('click', function () {
      localStorage.setItem(cacheKey, document.getElementById('textarea-level').value);
      runElm();
    }
  );

document.getElementById('edit-level')
  .addEventListener('click', function () {
      location.reload();
    }
  );

console.log('The start level: ');
console.log(startLevel);
if (startLevel !== null) {
  runElm();
}
