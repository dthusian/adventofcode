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

function fold(v) {
  if(v[0] === "x") {
    foldX(v[1]);
  } else if(v[0] === "y") {
    foldY(v[1]);
  } else {
    throw "what";
  }
}

function cullEmpty() {
  let maxx = 0;
  let maxy = 0;
  for(let x = 0; x < 1500; x++) {
    for(let y = 0; y < 1500; y++) {
      if(pointmap[x * 1500 + y]) {
        if(x > maxx) maxx = x;
        if(y > maxy) maxy = y;
      }
    }
  }
  return [maxx, maxy];
}

folds.forEach(fold);

const maxes = cullEmpty();

for(let y = 0; y < maxes[1] + 1; y++) {
  let s = "";
  for(let x = 0; x < maxes[0] + 1; x++) {
    s += pointmap[x * 1500 + y] ? "#" : " ";
  }
  console.log(s);
}