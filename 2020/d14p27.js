var input = require("fs").readFileSync("input.txt").toString().split("\r\n");

var maskE = BigInt(0);
var maskV = BigInt(0);
var mem = {};

function parseMask(str) {
  str = str.split("").reverse().join("");
  var e = BigInt(0), v = BigInt(0);
  for(var i = 0; i < str.length; i++) {
    switch(str.charAt(i)) {
      case "X": {
        break; // Do nothing
      }
      case "1": {
        e |= 1n << BigInt(i);
        v |= 1n << BigInt(i);
        break;
      }
      case "0": {
        e |= 1n << BigInt(i);
        // Don't set V, keep at 0
        break;
      }
    }
  }
  return [e, v];
}

for(var i = 0; i < input.length; i++) {
  var sym = input[i].split(" = ");
  var ref = sym[0];
  var val = sym[1];
  if(ref === "mask") {
    var _ = parseMask(val);
    maskE = _[0];
    maskV = _[1];
  } else if(ref.startsWith("mem")) {
    val = BigInt(val);
    val = val & (~maskE) | maskV; // Mask the value
    var addr = parseInt(ref.split("[")[1].split("]")[0]);
    mem[addr] = val;
  }
}

console.log(Object.values(mem).reduce((a, b) => a + b, 0n));