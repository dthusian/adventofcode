var input = require("fs").readFileSync("input.txt").toString().split("\r\n");

var qMask;
var sMask;
var mem = {};

function parseMask(str) {
  str = str.split("").reverse().join("");
  var q = BigInt(0), v = BigInt(0);
  for(var i = 0; i < str.length; i++) {
    switch(str.charAt(i)) {
      case "X": {
        q |= 1n << BigInt(i);
        break;
      }
      case "1": {
        v |= 1n << BigInt(i);
        break;
      }
      case "0": {
        break;
      }
    }
  }
  return [q, v];
}

function mkpos(x) {
  var pos = [];
  var i = 0;
  while(x) {
    if(x & 1n) pos.push(i);
    x >>= 1n;
    i++;
  }
  return pos;
}

function bitrepos(x, pos, maskbits, v) {
  var ret = 0n;
  var mask = 0n;
  for(var i = 0n; i < pos.length; i++) {
    ret |= ((v >> i) & 1n) << BigInt(pos[i]);
    mask |= 1n << BigInt(pos[i]);
  }
  return ret | (BigInt(x) & (~mask)) | maskbits;
}

function vquantscatter(addr, maskquant, maskbits, v) {
  var pos = mkpos(maskquant);
  for(var i = 0n; i < BigInt(Math.pow(2, pos.length)); i++) {
    var maskaddr = bitrepos(addr, pos, maskbits, i);
    mem[maskaddr] = BigInt(v);
  }
}

for(var i = 0; i < input.length; i++) {
  var sym = input[i].split(" = ");
  var ref = sym[0];
  var val = sym[1];
  if(ref === "mask") {
    var spl = parseMask(val);
    qMask = spl[0];
    sMask = spl[1];
  } else if(ref.startsWith("mem")) {
    var addr = parseInt(ref.split("[")[1].split("]")[0]);
    vquantscatter(addr, qMask, sMask, parseInt(val));
  }
}

console.log(Object.values(mem).reduce((a, b) => a + b, 0n));