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

// All actors will get a TransformComponent via the scene
// Therefor you do not need to set the TransformComponent on an entity
let game = {
  entities: [
    {
      name: 'player',
      components: [
        { type: 'render', data: { colors: [ '#4e9a06' ] } },
        { type: 'player-input' },
        { type: 'diamond-collector' },
        { type: 'rigid' },
        { type: 'can-squash' },
        { type: 'explodable' },
        { type: 'camera', data: { borderSize: 3} },
        { type: 'physics', data: { strength: 10, shape: 'square' } },
      ]
    },
    {
      name: 'rock',
      components: [
        { type: 'render', data: { colors: [ '#babdb6' ] } },
        { type: 'rigid' },
        { type: 'ai', data: { type: 'gravity' } },
        { type: 'physics', data: { strength: 20, shape: 'circle' } },
        { type: 'smash-down' },
      ]
    },
    {
      name: 'dynamite',
      components: [
        { type: 'render', data: { colors: [ '#cc0000' ] } },
        { type: 'rigid' },
        { type: 'ai', data: { type: 'gravity' } },
        { type: 'explodable' },
        { type: 'physics', data: { strength: 20, shape: 'circle' } },
        { type: 'smash-down' },
      ]
    },
    {
      name: 'explosion',
      components: [
        { type: 'render', data: { colors: [ '#cc0000', '#ce5c00', '#edd400' ], ticksPerColor: 2 } },
        { type: 'damage', data: { remainingTicks: 8, damageStrength: 80 } },
      ]
    },
    {
      name: 'enemy',
      components: [
        { type: 'render', data: { colors: [ '#ce5c00' ] } },
        { type: 'rigid' },
        { type: 'physics', data: { strength: 20, shape: 'circle' } },
        { type: 'ai', data: { type: 'walkaround' } },
        { type: 'explodable' },
        { type: 'trigger-explodable', data: { triggerStrength: 20 } },
      ]
    },
    {
      name: 'pet',
      components: [
        { type: 'render', data: { colors: [ '#75507b', '#ad7fa8' ], ticksPerColor: 8 } },
        { type: 'rigid' },
        { type: 'physics', data: { strength: 10, shape: 'circle' } },
        { type: 'ai', data: { type: 'walkaround' } },
        { type: 'explodable' },
      ]
    },
    {
      name: 'dirt',
      components: [
        { type: 'render', data: { colors: [ '#e9b96e' ] } },
        { type: 'rigid' },
        { type: 'squasable' },
        { type: 'physics', data: { strength: 1, shape: 'square' } },
      ]
    },
    {
      name: 'wall',
      components: [
        { type: 'render', data: { colors: [ '#626457' ] } },
        { type: 'rigid' },
        { type: 'physics', data: { strength: 50, shape: 'square' } },
      ]
    },
    {
      name: 'strongwall',
      components: [
        { type: 'render', data: { colors: [ '#000000' ] } },
        { type: 'rigid' },
        { type: 'physics', data: { strength: 100, shape: 'square' } },
      ]
    },
    {
      name: 'diamond',
      components: [
        { type: 'render', data: { colors: [ '#3465a4', '#729fcf' ], ticksPerColor: 12 } },
        { type: 'rigid' },
        { type: 'diamond' },
        { type: 'ai', data: { type: 'gravity' } },
        { type: 'physics', data: { strength: 100, shape: 'circle' } },
      ]
    }
  ],
  signs: {
    'p': 'player',
    'o': 'rock',
    '=': 'dynamite',
    'x': 'explosion',
    'e': 'enemy',
    'c': 'pet',
    '.': 'dirt',
    '|': 'wall',
    '#': 'strongwall',
    '*': 'diamond',
  },
  scene: defaultLevel.split("\n")
};


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
