var input = require("fs").readFileSync("input.txt").toString().split(/\r\n/gi);

var ops = input.map(v => {
  var spl = v.split(" ");
  return [spl[0], parseInt(spl[1])];
});

var execed = {};
var ip = 0, acc = 0;

while(true) {
  var op = ops[ip];
  if(execed[ip]) {
    console.log(acc);
    break;
  }
  execed[ip] = true;
  if(op[0] === "jmp") {
    ip += op[1];
  } else if(op[0] === "acc") {
    acc += op[1];
    ip++;
  } else if(op[0] === "nop") {
    ip++;
  }
}