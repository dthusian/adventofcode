var input = require("fs").readFileSync("input.txt").toString().split(/\r\n/gi).map(v => parseInt(v));

const preambleLength = 25;

function check(array, k) {
  var ret = false;
  for(var i = 0; i < preambleLength; i++) {
    for(var j = 0; j < preambleLength; j++) {
      var a = array[k - i - 1];
      var b = array[k - j - 1];
      if(a !== b && a + b === array[k]) {
        ret = true;
        break;
      }
    }
    if(ret) break;
  }
  return ret;
}

for(var i = preambleLength; i < input.length; i++) {
  if(!check(input, i)) {
    console.log(input[i]);
    break;
  }
}