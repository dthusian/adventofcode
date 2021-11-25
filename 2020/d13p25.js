let input = require("fs").readFileSync("input.txt").toString().split("\r\n");

let departTime = parseInt(input[0]);
let busses = input[1].split(",").map(v => parseInt(v)).filter(v => !isNaN(v));

let i;
let busnum;
for(i = departTime; ; i++) {
  let brk = false;
  for(var j = 0; j < busses.length; j++) {
    if(i % busses[j] === 0) {
      busnum = busses[j];
      brk = true;
      break;
    }
  }
  if(brk) break;
}

console.log((i - departTime) * busnum);