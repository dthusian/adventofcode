let matrix = require("fs").readFileSync("input.txt").toString().split(/[\n\r]+/); // [y][x]

function checkDiag(vx, vy) {
  let trees = 0;
  let x = 0;
  for(let y = 0; y < matrix.length; y += vy) {
    if(matrix[y][x] === "#") trees++;
    x += vx;
    x %= matrix[y].length;
  }
  return trees;
}

function mul(...args) {
  console.log(args.reduce((v, a) => v * a));
}

mul(
  checkDiag(1, 1),
  checkDiag(3, 1),
  checkDiag(5, 1),
  checkDiag(7, 1),
  checkDiag(1, 2)
);