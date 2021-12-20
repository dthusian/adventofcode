const input = require("fs").readFileSync("input.txt").toString().split("x=")[1].split(", y=").map(v => v.split("..")).flat().map(v => parseInt(v));

function checkXv(vx) {
  let cx = 0;
  let t = 0;
  let lastTinside = 0;
  while(true) {
    cx += vx;
    vx -= 1 * Math.sign(vx);
    t++;
    if(cx >= input[0] && cx <= input[1]) {
      lastTinside = t;
    }
    if(vx === 0) {
      if(cx >= input[0] && cx <= input[1]) {
        // Stopped inside
        return Infinity;
      }
      if(Math.abs(cx) < Math.abs(input[0])) {
        // Stopped before
        return -1;
      }
      if(Math.abs(cx) > Math.abs(input[1])) {
        // Stopped outside
        return lastTinside;
      }
    }
  }
}

function checkFull(vx, vy) {
  let cx = 0;
  let cy = 0;
  let maxy = 0;
  while(true) {
    cx += vx;
    cy += vy;
    maxy = Math.max(maxy, cy);
    vx -= 1 * Math.sign(vx);
    vy -= 1;
    if(cx >= input[0] && cx <= input[1] && cy >= input[2] && cy <= input[3]) {
      return maxy;
    } else {
      if(cy < input[2]) {
        return -1;
      }
    }
  }
}

let maxXv = 0;
let viableXvs = [];

for(let i = 1; i < 1000; i++) {
  const c = checkXv(i);
  if(c > 0) {
    viableXvs.push([i, c]);
  }
}

console.log(viableXvs.map(v => {
  let maxy = -1;
  for(let i = 0; i < 500; i++) {
    maxy = Math.max(maxy, checkFull(v[0], i));
  }
  return maxy;
}).reduce((a, b) => Math.max(a, b)));