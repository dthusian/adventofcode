// [y][x]
var input = require("fs").readFileSync("input.txt").toString().split("\r\n").map(v => v.split(""));

var state = input;
var oldstate = input;

var width = input[0].length, height = input.length;

function reallyIsNaN(x) { return typeof x === "number" && isNaN(x); }
function isUnsafe(x) { return reallyIsNaN(x) || x === null || x === undefined; }

function safeGet(arr, x, y) {
  return (isUnsafe(arr[y]) ? [] : arr[y])[x];
}

function raytrace(x, y, vx, vy) {
  do {
    x += vx;
    y += vy;
  } while(safeGet(oldstate, x, y) === ".");
  return safeGet(oldstate, x, y);
}

function adj(i, j) {
  return [
    raytrace(i, j, -1, -1),
    raytrace(i, j, -1,  0),
    raytrace(i, j, -1, +1),
    raytrace(i, j,  0, -1),
    raytrace(i, j,  0, +1),
    raytrace(i, j, +1, -1),
    raytrace(i, j, +1,  0),
    raytrace(i, j, +1, +1)
  ].filter(v => v);
}

function countOccupieds(arr) {
  return arr.filter(v => v === "#").length;
}

function step() {
  oldstate = JSON.parse(JSON.stringify(state));
  for(var i = 0; i < width; i++) {
    for(var j = 0; j < height; j++) {
      switch(oldstate[j][i]) {
        case ".": {
          state[j][i] = ".";
          break;
        }
        case "L": {
          var cnt = countOccupieds(adj(i, j));
          if(cnt === 0) {
            state[j][i] = "#";
          } else {
            state[j][i] = "L";
          }
          break;
        }
        case "#": {
          var cnt = countOccupieds(adj(i, j));
          if(cnt >= 5) {
            state[j][i] = "L";
          } else {
            state[j][i] = "#";
          }
          break;
        }
        default: {
          state[j][i] = "?";
          break;
        }
      }
    }
  }
}

function cmpState() {
  var ret = false;
  for(var i = 0; i < width; i++) {
    for(var j = 0; j < height; j++) {
      if(oldstate[j][i] !== state[j][i]) {
        ret = true;
        break;
      }
    }
    if(ret) break;
  }
  return ret;
}

do { step(); } while (cmpState());
console.log(state.flat().filter(v => v === "#").length);