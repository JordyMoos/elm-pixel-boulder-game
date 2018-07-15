'use strict';

require('./static/style.css');
const Elm = require('./elm/Main.elm');
const defaultGame = require('./static/level-001');


document.getElementById('textarea-level').value =
  localStorage.getItem('advanced-level') || JSON.stringify(defaultGame, null, 2);


document.getElementById('reset-level')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = JSON.stringify(defaultGame, null, 2);
  });
document.getElementById('submit-level')
  .addEventListener('click', function () {
      document.getElementById('editor-container').style.display = 'none';
      document.getElementById('game-container').style.display = '';

      localStorage.setItem('advanced-level', document.getElementById('textarea-level').value);

      Elm.Main.embed(
        document.getElementById('elm'),
        JSON.parse(document.getElementById('textarea-level').value)
      );
    }
  );


document.getElementById('edit-level')
  .addEventListener('click', function () {
      location.reload();
    }
  );