var input = require("fs").readFileSync("input.txt").toString().split(/\r\n/gi).map(v => parseInt(v)).sort((a, b) => a - b);

function iterkeys(o, fn) {
  var keys = Object.keys(o);
  for(var i = 0; i < keys.length; i++) {
    fn(keys[i], o[keys[i]]);
  }
}

function num(v) { return typeof v === "number"; }
function clone(o) { return JSON.parse(JSON.stringify(o)); }

function doit(arr) {
  var currentEnds = { 0: 1 };
  for(var i = 0; i < arr.length; i++) {
    var newEnds = {};
    if(i === arr.length - 1) debugger;
    iterkeys(currentEnds, (k, v) => {
      if(Math.abs(k - arr[i]) <= 3) {
        // Can connect next
        // Fork the thing, double the configurations and make half not connect to the next

        // Fork 1: Here we make them connect to next
        if(!newEnds[arr[i]]) newEnds[arr[i]] = 0;
        newEnds[arr[i]] += v;

        // Fork 2: Here we make them also not connect
        if(!newEnds[k]) newEnds[k] = 0;
        newEnds[k] = v;
      }
      // The fork dies if it can no longer connect to next
      // Branches that are unable to connect to next are not forked
      // and not copied to newEnds, thereby killing them
    });
    currentEnds = newEnds;
  }
  return currentEnds;
}

var inputjoltage = input[input.length - 1] + 3
input.push(inputjoltage);

var res = doit(input);
console.log(res[inputjoltage]);