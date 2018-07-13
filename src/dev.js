'use strict';

require('./static/style.css');
const Elm = require('./elm/Main.elm');


let defaultLevel = (
  "#########################################\n" +
  "#.......................................#\n" +
  "#.......................          ......#\n" +
  "#|||||||||..........P...E         ......#\n" +
  "#|**| O  O..............................#\n" +
  "#|**| || |....OOOOOOOOO.................#\n" +
  "#|**| || |.....OOOOOOO........*.........#\n" +
  "#|**| || |......OOOOO...................#\n" +
  "#|**| ||=|...................*..........#\n" +
  "#|**| ||||.....       ..................#\n" +
  "#|**| ||*|....         ..........*......#\n" +
  "#|**|E||*|...           ................#\n" +
  "#|||||||||..............................#\n" +
  "#...................*...................#\n" +
  "#.......................................#\n" +
  "#....*.........................*........#\n" +
  "#............*..........................#\n" +
  "#.......................................#\n" +
  "#########################################\n"
);


document.getElementById('textarea-level').value =
  localStorage.getItem('level') || defaultLevel;


document.getElementById('reset-level')
  .addEventListener('click', function () {
    document.getElementById('textarea-level').value = defaultLevel;
  });
document.getElementById('submit-level')
  .addEventListener('click', function () {
    document.getElementById('editor-container').style.display = 'none';
    document.getElementById('game-container').style.display = '';

    localStorage.setItem('level', document.getElementById('textarea-level').value);

    Elm.Main.embed(
      document.getElementById('elm'),
      {
        debug: true,
        scene: document.getElementById('textarea-level').value.split("\n")
      }
    );
  }
);


document.getElementById('edit-level')
  .addEventListener('click', function () {
      location.reload();
  }
);
