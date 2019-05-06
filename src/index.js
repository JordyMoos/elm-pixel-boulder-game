'use strict';

const {Elm} = require('./elm/Main.elm');
const easyCacheKey = 'easy-level-cache-4';
const advancedCacheKey = 'level-cache-4';

const defaultLevel = require('./static/levels/test/pixel.json');
const levels = {
  'level-pixel': defaultLevel,
  'level-nes-small': require('./static/levels/test/nes-small.json'),
  'level-nes': require('./static/levels/test/nes.json'),
  'level-image': require('./static/levels/test/images.json'),
  'level-pacman': require('./static/levels/test/pacman.json'),
  'level-tank': require('./static/levels/test/tank.json'),
  'level-game-of-life': require('./static/levels/test/game-of-life.json'),
};

const sampleLevel = require('./static/levels/test/sample.json');
const defaultEasyScene = `
 ..o...*...
 .p...***..
 ......*...
 ..........`;


const urlParams = new URLSearchParams(window.location.search);
const startLevel = urlParams.get('startLevel') || null;
const hideDebug = !! urlParams.get('hideDebug');
let editorMode = urlParams.get('editorMode') === 'advanced' ? 'advanced' : 'easy';
let notEditorMode = getOtherMode(editorMode);
let subMode = 'image';

function sceneStringToMap(scene) {
  return scene.split("\n").map(row => row.split(''));
}

function getOtherMode(mode) {
  return mode === 'easy' ? 'advanced' : 'easy';
}

function showProperSubMode() {
  if (subMode === 'image') {
    document.getElementById('canvas-mode').style.display = '';
    document.getElementById('ascii-mode').style.display = 'none';
  } else {
    document.getElementById('canvas-mode').style.display = 'none';
    document.getElementById('ascii-mode').style.display = '';
  }
}

function combineAsciiScenes(big, other) {
  let dest = [];

  [big, other].forEach(sceneMap => {
    sceneMap.forEach((rows, y) => {
      dest[y] = dest[y] || [];
      rows.forEach((sign, x) => {
        dest[y][x] = sign;
      });
    });
  });

  return dest;
}

['easy', 'advanced'].forEach(function(mode) {
  document.getElementById(mode + '-editor-option').addEventListener('change', function (data) {
    editorMode = data.target.value;
    notEditorMode = getOtherMode(editorMode);

    document.getElementById(editorMode + '-editor').style.display = '';
    document.getElementById( notEditorMode + '-editor').style.display = 'none';
  });
});

document.getElementById(editorMode + '-editor-option').checked = 'checked';
document.getElementById(editorMode + '-editor').style.display = '';
document.getElementById(notEditorMode + '-editor').style.display = 'none';

let imageLoadedInfo = {};
let activeSpriteElement = undefined;
let spriteElements = Array.from(document.getElementById('sprite-container').getElementsByTagName('img'));
spriteElements.forEach(function (element) {
  let elementId = element.id;
  imageLoadedInfo[elementId] = element.complete;
  element.addEventListener('load', function() {
    imageLoadedInfo[elementId] = true;

    drawIfEverythingIsLoaded();
  });

  element.addEventListener('click', function (event) {
    spriteElements.forEach(function (aSpriteElement) {
      aSpriteElement.className = '';
    });

    element.className = 'active';
    activeSpriteElement = element;
    updateSpriteDescription();
  });
});
spriteElements[0].className = 'active';
activeSpriteElement = spriteElements[0];
updateSpriteDescription();

function updateSpriteDescription() {
  document.getElementById('active-sprite-description').innerText = activeSpriteElement.dataset.description;
}

let backgroundSpriteElement = document.getElementById('sprite- ');
let canvasContainerElement = document.getElementById('canvas-editor-container');
let canvasElement = document.getElementById('canvas-editor-canvas');
let canvas = canvasElement.getContext('2d');
let tileSize = 24;
let canvasWidth = (tileSize + 1) * 50;
let canvasHeight = (tileSize + 1) * 50;
canvasElement.width = canvasWidth;
canvasElement.height = canvasHeight;
let totalXRows = Math.ceil(canvasWidth / tileSize);
let totalYRows = Math.ceil(canvasHeight / tileSize);

let emptyAsciiScene = Array(totalYRows).fill(backgroundSpriteElement.dataset.sign)
  .map(y => Array(totalXRows).fill(backgroundSpriteElement.dataset.sign));

var asciiScene = combineAsciiScenes(
  emptyAsciiScene,
  sceneStringToMap(localStorage.getItem(easyCacheKey) || defaultEasyScene)
);


function fullRedraw() {
  canvas.clearRect(0, 0, canvasWidth, canvasHeight);

  [...Array(totalYRows).keys()].slice(1).forEach(function (yId) {
    let y = yId * (tileSize + 1);
    canvas.beginPath();
    canvas.moveTo(0, y);
    canvas.lineTo(canvasWidth, y);
    canvas.stroke();

    [...Array(totalXRows).keys()].slice(1).forEach(function (xId) {
      let x = xId * (tileSize + 1);
      canvas.beginPath();
      canvas.moveTo(x, 0);
      canvas.lineTo(x, canvasHeight);
      canvas.stroke();

      let sign = asciiScene[yId - 1] && asciiScene[yId - 1][xId - 1] ? asciiScene[yId - 1][xId - 1] : ' ';
      let elementSprite = document.getElementById('sprite-' + sign) || backgroundSpriteElement;
      canvas.drawImage(backgroundSpriteElement, x - tileSize - 1, y - tileSize - 1, tileSize, tileSize);
      canvas.drawImage(elementSprite, x - tileSize - 1, y - tileSize - 1, tileSize, tileSize);

      asciiScene[yId - 1][xId - 1] = sign;
    });
  });

  canvasElement.addEventListener('click', function (event) {
    let x = event.clientX - canvasElement.offsetLeft + document.body.scrollLeft + document.documentElement.scrollLeft + canvasContainerElement.scrollLeft;
    let y = event.clientY - canvasElement.offsetTop + document.body.scrollTop + document.documentElement.scrollTop + canvasContainerElement.scrollTop;

    x = x - (x % (tileSize + 1));
    y = y - (y % (tileSize + 1));

    canvas.drawImage(backgroundSpriteElement, x, y, tileSize, tileSize);
    canvas.drawImage(activeSpriteElement, x, y, tileSize, tileSize);

    let yId = Math.floor(y / (tileSize + 1));
    let xId = Math.floor(x / (tileSize + 1));

    asciiScene[yId][xId] = activeSpriteElement.dataset.sign;
  });
}

function drawIfEverythingIsLoaded() {
  let allLoaded = Object.keys(imageLoadedInfo).every(function (id) {
    return imageLoadedInfo[id];
  });

  if (allLoaded) {
    document.getElementById('easy-editor-loader').style.display = 'none';
    fullRedraw();
  }
}

showProperSubMode();
drawIfEverythingIsLoaded();

document.getElementById('easy-to-ascii').addEventListener('click', function () {
  let scene = asciiScene.map(row => {
    return row.join('').trimRight();
  }).join("\n");

  document.getElementById('easy-textarea-level').value = scene;

  subMode = 'ascii';
  showProperSubMode();
});

document.getElementById('easy-to-image').addEventListener('click', function () {
  let sceneString = document.getElementById('easy-textarea-level').value;
  let scene = sceneStringToMap(sceneString);

  asciiScene = combineAsciiScenes(emptyAsciiScene, scene);
  fullRedraw();

  subMode = 'image';
  showProperSubMode();
});

function runElm() {
  document.getElementById('editor-container').style.display = 'none';
  document.getElementById('game-container').style.display = '';

  try {

    let jsonLevel;
    if (editorMode === 'advanced') {
      jsonLevel = JSON.parse(document.getElementById('advanced-textarea-level').value);
    } else {
      let sceneString = document.getElementById('easy-textarea-level').value;
      let scene = sceneString.split("\n");
      let diamondCount = sceneString.split("*").length - 1;
      jsonLevel = sampleLevel;
      jsonLevel.scene = scene;
      jsonLevel.subscribers[1].inventoryUpdatedData.minimumQuantity = diamondCount;
    }

    let app = Elm.Main.init({
      node: document.getElementById('elm'),
      flags: {
        jsonLevel: jsonLevel,
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
  localStorage.getItem(advancedCacheKey) || JSON.stringify(defaultLevel, null, 2);

document.getElementById('easy-textarea-level').value =
  localStorage.getItem(easyCacheKey) || defaultEasyScene;

Object.keys(levels).forEach(function(id) {
  document.getElementById(id)
    .addEventListener('click', function () {
      document.getElementById('advanced-textarea-level').value = JSON.stringify(levels[id], null, 2);
    });
});

document.getElementById('submit-level')
  .addEventListener('click', function () {
      if (subMode === 'image') {
        let scene = asciiScene.map(row => {
          return row.join('').trimRight();
        }).join("\n");

        document.getElementById('easy-textarea-level').value = scene;
      }

      localStorage.setItem(advancedCacheKey, document.getElementById('advanced-textarea-level').value);
      localStorage.setItem(easyCacheKey, document.getElementById('easy-textarea-level').value);
      runElm();
    }
  );

document.getElementById('edit-level')
  .addEventListener('click', function () {
    urlParams.delete('startLevel');
    location.replace(location.origin + location.pathname + '?' + urlParams.toString());
    }
  );

if (startLevel !== null) {
  runElm();
}
