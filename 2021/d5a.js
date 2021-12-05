const input = require("fs").readFileSync("input.txt").toString().split(/[\r\n]+/g).map(v => v.split(" -> "));

function identifyLine(a, b) {
  if(a[0] === b[0]) return "vertical";
  if(a[1] === b[1]) return "horizontal";
  return "bad";
}

function fillHori(matrix, a, b, y) {
  const mi = Math.min(a, b);
  const mx = Math.max(a, b);
  for(let i = mi; i <= mx; i++) {
    matrix[i + y * 1000]++;
  }
}

function fillVerti(matrix, a, b, x) {
  const mi = Math.min(a, b);
  const mx = Math.max(a, b);
  for(let i = mi; i <= mx; i++) {
    matrix[i * 1000 + x]++;
  }
}

// [x + y * 1000]
const matrix = (new Array(1000 * 1000)).fill(0);
for(let i = 0; i < input.length; i++) {
  const a = input[i][0].split(",").map(v => parseInt(v));
  const b = input[i][1].split(",").map(v => parseInt(v));
  const type = identifyLine(a, b);
  if(type === "bad") continue;
  if(type === "horizontal") {
    fillHori(matrix, a[0], b[0], a[1]);
  }
  if(type === "vertical") {
    fillVerti(matrix, a[1], b[1], a[0]);
  }
}
console.log(matrix.flat().filter(v => v >= 2).length);