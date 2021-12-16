const input = require("fs").readFileSync("input.txt").toString().split(/[\r\n]+/g).map(v => v.split("").map(v => parseInt(v)));

function printmat(v) {
  for(let y = 0; y < 10; y++) {
    console.log(v[y].join("").substring(0, 10));
  }
}

function tryInc(v, x, y) {
  if(x < 0 || x > 9 || y < 0 || y > 9) {
    return;
  }
  v[y][x]++;
}

function step(v) {
  for(let y = 0; y < 10; y++) {
    for(let x = 0; x < 10; x++) {
      v[y][x]++;
    }
  }
  let flashes = 0;
  while(!v.flat().every(v => isNaN(v) || v < 10)) {
    for(let y = 0; y < 10; y++) {
      for(let x = 0; x < 10; x++) {
        if(!isNaN(v[y][x]) && v[y][x] > 9) {
          v[y][x] = NaN;
          tryInc(v, x - 1, y - 1);
          tryInc(v, x - 1, y    );
          tryInc(v, x - 1, y + 1);
          tryInc(v, x    , y - 1);
          tryInc(v, x    , y + 1);
          tryInc(v, x + 1, y - 1);
          tryInc(v, x + 1, y    );
          tryInc(v, x + 1, y + 1);
          flashes++;
        }
      }
    }
  }
  for(let y = 0; y < 10; y++) {
    for(let x = 0; x < 10; x++) {
      if(isNaN(v[y][x])) {
        v[y][x] = 0;
      }
    }
  }
  return flashes;
}

let i = 0;
while(true) {
  i++;
  if(step(input) === 100) {
    console.log(i);
    break;
  }
}