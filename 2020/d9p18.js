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

var inv;

for(var i = preambleLength; i < input.length; i++) {
  if(!check(input, i)) {
    inv = input[i];
    break;
  }
}

var dp = [];
var ret = false;

for(var i = 0; i < input.length; i++) {
  // Increment dp numbers
  for(var j = 0; j < dp.length; j++) {
    var cdp = dp[j];
    cdp.sum += input[i];
    if(cdp.min > input[i]) cdp.min = input[i];
    if(cdp.max < input[i]) cdp.max = input[i];
    if(cdp.sum === inv) {
      console.log(cdp.min + cdp.max);
      ret = true;
      break;
    }
  }
  if(ret) break;
  // Make new entry
  dp.push({ sum: input[i], min: input[i], max: input[i] });
}