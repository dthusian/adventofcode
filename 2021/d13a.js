let [points, folds] = require("fs").readFileSync("input.txt").toString().split(/\r\n\r\n|\n\n/g);
let pointmap = (new Array(1500 * 1500)).fill(false); // [x * 1500 + y];

points = points.split(/\r\n|\n/g).map(v => v.split(",").map(v2 => parseInt(v2)));
folds = folds.split(/\r\n|\n/g).map(v => v.split("=")).map(v => [v[0].charAt(v[0].length - 1), parseInt(v[1])]);
points.forEach(v => {
  pointmap[v[0] * 1500 + v[1]] = true;
});

function foldY(v) {
  function copyRow(a, b) {
    for(let i = 0; i < 1500; i++) { // i = x
      pointmap[i * 1500 + a] = pointmap[i * 1500 + a] || pointmap[i * 1500 + b];
      pointmap[i * 1500 + b] = false;
    }
  }

  for(let i = v, mi = v; i < 1500 && mi >= 0; i++, mi--) {
    copyRow(mi, i);
  }
}

function foldX(v) {
  function copyColumn(a, b) {
    for(let i = 0; i < 1500; i++) { // i = x
      pointmap[a * 1500 + i] = pointmap[a * 1500 + i] || pointmap[b * 1500 + i];
      pointmap[b * 1500 + i] = false;
    }
  }

  for(let i = v, mi = v; i < 1500 && mi >= 0; i++, mi--) {
    copyColumn(mi, i);
  }
}

if(folds[0][0] === "x") {
  foldX(folds[0][1]);
} else {
  foldY(folds[0][1]);
}

console.log(pointmap.filter(v => v).length);