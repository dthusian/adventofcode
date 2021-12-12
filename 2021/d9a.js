const input = require("fs").readFileSync("input.txt").toString().split(/\r*\n/g).map(v => v.split("").map(v2 => parseInt(v2)));
let sum = 0;

for(let y = 0; y < input.length; y++) {
  for(let x = 0; x < input[0].length; x++) {
    const pp = [];
    if(y !== 0) {
      pp.push(input[y - 1][x]);
    }
    if(y !== input.length - 1) {
      pp.push(input[y + 1][x]);
    }
    if(x !== 0) {
      pp.push(input[y][x - 1]);
    }
    if(x !== input[0].length - 1) {
      pp.push(input[y][x + 1]);
    }
    if(pp.every(v => v > input[y][x])) {
      sum += 1 + input[y][x];
    }
  }
}

console.log(sum);