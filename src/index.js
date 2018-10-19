'use strict';

require('./static/style.css');
const {Elm} = require('./elm/Main.elm');
const levelPixel = require('./static/levels/test/pixel.json');
const levelNesSmall = require('./static/levels/test/nes-small.json');
const levelNes = require('./static/levels/test/nes.json');
const levelImage = require('./static/levels/test/images.json');
const levelPacman = require('./static/levels/test/pacman.json');
const levelTank = require('./static/levels/test/tank.json');
const levelGameOfLife = require('./static/levels/test/game-of-life.json');

console.log(Elm);


document.getElementById('textarea-level').value =
  localStorage.getItem('advanced-level') || JSON.stringify(levelPixel, null, 2);

document.getElementById('level-pixel')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = JSON.stringify(levelPixel, null, 2);
  });
document.getElementById('level-nes-small')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = JSON.stringify(levelNesSmall, null, 2);
  });
document.getElementById('level-nes')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = JSON.stringify(levelNes, null, 2);
  });
document.getElementById('level-image')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = JSON.stringify(levelImage, null, 2);
  });
document.getElementById('level-pacman')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = JSON.stringify(levelPacman, null, 2);
  });
document.getElementById('level-tank')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = JSON.stringify(levelTank, null, 2);
  });
document.getElementById('level-game-of-life')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = JSON.stringify(levelGameOfLife, null, 2);
  });

document.getElementById('submit-level')
  .addEventListener('click', function () {
      document.getElementById('editor-container').style.display = 'none';
      document.getElementById('game-container').style.display = '';

      localStorage.setItem('advanced-level', document.getElementById('textarea-level').value);

      try {

        console.log(Elm);
        Elm.Main.init({
            node: document.getElementById('elm'),
            flags: JSON.parse(document.getElementById('textarea-level').value)
        });
      } catch (e) {
        console.dir(e);
        document.getElementById('elm').innerText = 'Error: ' + e.message;
      }
    }
  );


document.getElementById('edit-level')
  .addEventListener('click', function () {
      location.reload();
    }
  );