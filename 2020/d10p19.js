var input = require("fs").readFileSync("input.txt").toString().split(/\r\n/gi).map(v => parseInt(v));

var sorted = input.sort((a, b) => a - b);

var d1count = 0, d3count = 0;
var last = 0;
for(var i = 0; i < sorted.length; i++) {
  var diff = Math.abs(sorted[i] - last);
  if(diff === 1) d1count++;
  if(diff === 3) d3count++;
  last = sorted[i];
}
d3count++;

console.log(d1count * d3count);