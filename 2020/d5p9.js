var input = require("fs").readFileSync("input.txt").toString().split("\r\n");

function translatePass(pass) {
  var bits = 0;
  for(var i = 0; i < pass.length; i++) {
    bits <<= 1;
    if(pass[i] === "B" || pass[i] === "R") {
      bits |= 1;
    }
  }
  return bits;
}

console.log(input.reduce((a, v) => Math.max(translatePass(v), a), 0));