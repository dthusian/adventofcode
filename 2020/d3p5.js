var matrix = require("fs").readFileSync("input.txt").toString().split(/[\n\r]+/); // [y][x]

var trees = 0;
var x = 0;
for(var i = 0; i < matrix.length; i++) {
  if(matrix[i][x] === "#") trees++;
  x += 3;
  x %= matrix[i].length;
}

console.log(trees);