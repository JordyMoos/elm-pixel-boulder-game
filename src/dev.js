'use strict';

require('./static/style.css');
const Elm = require('./elm/Main.elm');
const level001 = require('./static/level-001');
const levelPacman = require('./static/level-pacman');


document.getElementById('textarea-level').value =
  localStorage.getItem('advanced-level') || JSON.stringify(level001, null, 2);

document.getElementById('level-001')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = JSON.stringify(level001, null, 2);
  });
document.getElementById('level-pacman')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = JSON.stringify(levelPacman, null, 2);
  });

document.getElementById('submit-level')
  .addEventListener('click', function () {
      document.getElementById('editor-container').style.display = 'none';
      document.getElementById('game-container').style.display = '';

      localStorage.setItem('advanced-level', document.getElementById('textarea-level').value);

      try {

        Elm.Main.embed(
          document.getElementById('elm'),
          JSON.parse(document.getElementById('textarea-level').value)
        );
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