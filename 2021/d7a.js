const input = require("fs").readFileSync("input.txt").toString().split(",").map(v => parseInt(v));

let min = Infinity;

for(let i = 0; i < 2000; i++) {
  const fuel = input.map(v => Math.abs(v - i)).reduce((a, b) => a + b);
  min = Math.min(fuel, min);
}

console.log(min);