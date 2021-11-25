var input = require("fs").readFileSync("input.txt").toString().split(",").map(v => parseInt(v));

var dp = input;

function extractLast2(n) {
  var i1 = dp.lastIndexOf(n);
  var i2 = dp.slice(0, i1).lastIndexOf(n);
  return [i1, i2];
}

for(var i = dp.length; i < 2020; i++) {
  var last = dp[dp.length - 1];
  var last2 = extractLast2(last);
  if(last2[1] === -1) {
    dp.push(0);
  } else {
    dp.push(last2[0] - last2[1]);
  }
}

console.log(dp[2019]);