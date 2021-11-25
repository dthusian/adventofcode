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

var seats = new Array(1024);
input.map(translatePass).forEach(v => void (seats[v] = true));

var initrun = true;

for(var i = 0; i < 1024; i++) {
  if(initrun) {
    if(seats[i]) {
      initrun = false;
    }
  } else {
    if(!seats[i]) {
      console.log(i);
      break;
    }
  }
}