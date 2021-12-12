const input = require("fs").readFileSync("input.txt").toString().split(",").map(v => parseInt(v));

function tick(state) {
  const newstate = Array.from(state);
  state.forEach((v, i) => {
    if(v === 0) {
      newstate.push(8);
      newstate[i] = 6;
    } else {
      newstate[i] = v - 1;
    }
  });
  return newstate;
}

let state = input;
for(let i = 0; i < 80; i++) {
  state = tick(state);
}
console.log(state.length);