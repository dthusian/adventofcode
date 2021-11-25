var input = require("fs").readFileSync("input.txt").toString();
var nums = input.split(/[\n\r]+/gi).map(v => parseInt(v));

for(var i = 0; i < nums.length; i++) {
  for(var j = 0; j < nums.length; j++) {
    for(var k = 0; k < nums.length; k++) {
      if(nums[i] + nums[j] + nums[k] === 2020) {
        console.log(nums[i] * nums[j] * nums[k]);
      }
    }
  }
}