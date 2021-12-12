const input = require("fs").readFileSync("input.txt").toString().split(",").map(v => parseInt(v));
const state = (new Array(9)).fill(0).map((_, i) => BigInt(input.filter(v => v === i).length));

function tick(state) {
  const z = state.shift();
  state.push(z);
  state[6] += z;
}

for(let i = 0; i < 256; i++) {
  tick(state);
}

console.log(state.reduce((a, b) => a + b));