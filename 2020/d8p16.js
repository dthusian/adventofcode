var input = require("fs").readFileSync("input.txt").toString().split(/\r\n/gi);

var ops = input.map(v => {
  var spl = v.split(" ");
  return { opcode: spl[0], imm1: parseInt(spl[1]) };
});

function deepcopy(o) {
  return o.map(v => ({ opcode: v.opcode, imm1: v.imm1 }));
}

function run(program) {
  var execed = {};
  var ip = 0, acc = 0;
  var ret;
  while(true) {
    var op = program[ip];
    if(!op) {
      ret = { inf: false, acc: acc };
      break;
    }
    if(execed[ip]) {
      ret = { inf: true, acc: acc };
      break;
    }
    execed[ip] = true;
    if(op.opcode === "jmp") {
      ip += op.imm1;
    } else if(op.opcode === "acc") {
      acc += op.imm1;
      ip++;
    } else if(op.opcode === "nop") {
      ip++;
    }
  }
  return ret;
}

for(var i = 0; i < ops.length; i++) {
  console.log("i: " + i);
  if(ops[i].opcode === "acc") {
    continue;
  }

  var clone = deepcopy(ops);
  if(ops[i].opcode === "jmp") {
    clone[i].opcode = "nop";
  } else if(ops[i].opcode === "nop") {
    clone[i].opcode = "jmp";
  }

  var res = run(clone);

  if(!res.inf) {
    console.log(res.acc);
    break;
  }
}