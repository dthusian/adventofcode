var input = require("fs").readFileSync("input.txt").toString().split(/\r\n/ig);
var rules = {};
var checkCache = {};

input.forEach(v => {
  var s = v.substring(0, v.length - 1).split(" ");
  var rname = s[0] + " " + s[1];
  var rule = [];
  for(var i = 5; i < s.length; i += 4) {
    rule.push(s[i] + " " + s[i + 1]);
  }
  if(rule[0] === "other bags") {
    rules[rname] = [];
  } else { 
    rules[rname] = rule;
  }
});

function recurCheckCol(start, target) {
  const ccIndex = start + "|" + target;
  if(checkCache[ccIndex] !== undefined) return checkCache[ccIndex];
  if(!rules[start]) throw new Error("No rules found");
  var allowed = rules[start];
  var ret = false;
  for(var i = 0; i < allowed.length; i++) {
    if(allowed[i] === target || recurCheckCol(allowed[i], target)) {
      ret = true;
      break;
    }
  }
  checkCache[ccIndex] = ret;
  return ret;
}

console.log(Object.keys(rules).filter(rule => recurCheckCol(rule, "shiny gold")).length);