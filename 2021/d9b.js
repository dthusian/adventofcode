const input = require("fs").readFileSync("input.txt").toString().split(/\r*\n/g).map(v => v.split("").map(v2 => parseInt(v2)));

function isViable(v) {
  return !isNaN(v) && v < 9 && v >= 0;
}

function floodfill(x, y) { 
  let size = 0;
  const queue = [[x, y]];
  while(queue.length) {
    const [cx, cy] = queue.shift();
    if(!isViable(input[cy][cx])) continue;
    if(cx > 0 && isViable(input[cy][cx - 1])) {
      queue.push([cx - 1, cy]);
    }
    if(cx < input[0].length - 1 && isViable(input[cy][cx + 1])) {
      queue.push([cx + 1, cy]);
    }
    if(cy > 0 && isViable(input[cy - 1][cx])) {
      queue.push([cx, cy - 1]);
    }
    if(cy < input.length - 1 && isViable(input[cy + 1][cx])) {
      queue.push([cx, cy + 1]);
    }
    size++;
    input[cy][cx] = -1;
  }
  return size;
}

const basins = [];

for(let y = 0; y < input.length; y++) {
  for(let x = 0; x < input[0].length; x++) {
    if(isViable(input[y][x])) {
      basins.push(floodfill(x, y));
    }
  }
}

console.log(basins.sort((a, b) => b - a).slice(0, 3).reduce((a, b) => a * b));