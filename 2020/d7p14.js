var input = require("fs").readFileSync("input.txt").toString().split(/\r\n/ig);
var rules = {};
var countCache = {};

input.forEach(v => {
  var s = v.split(" ");
  var rname = s[0] + " " + s[1];
  var rule = {};
  for(var i = 5; i < s.length; i += 4) {
    if(s[i - 1] === "no") break;
    rule[s[i] + " " + s[i + 1]] = parseInt(s[i - 1]);
  }
  rules[rname] = rule;
});

function countBags(target) {
  var count = 1; // Include self
  if(countCache[target]) return countCache[target];
  var rule = rules[target];
  var keys = Object.keys(rule);
  for(var i = 0; i < keys.length; i++) {
    var key = keys[i];
    count += rule[key] * countBags(key);
  }
  return count;
}

console.log(countBags("shiny gold") - 1);