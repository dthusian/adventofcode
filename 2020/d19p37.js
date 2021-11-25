// type rule = { type: "simple", value: string } | { type: "complex", value: number[][] }
var rules = {};

var input = require("fs").readFileSync("input.txt").toString().split("\r\n\r\n");

input[0].split("\r\n").forEach(v => {
  var spl0 = v.split(": ");
  var spl1 = spl0[1].split(" | ");
  if(spl1[0].charAt(0) === "\"") {
    rules[parseInt(spl0[0])] = { type: "simple", value: spl1[0].charAt(1) };
  } else {
    rules[parseInt(spl0[0])] = { type: "complex", value: spl1.map(v => v.split(" ").map(v => parseInt(v))) };
  }
});

var cases = input[1].split("\r\n");

// type stackstate = { rule: number, pos: number, branch: number }
// type thread = { pos: number, stack: stackstate[] }
var state = [{
  pos: 0,
  stack: [{ rule: 0, pos: 0, branch: 0 }],
  finished: false
}];
var master = "";

function digestSymbol() {
  var len = state.length;
  if(len === 0) {
    state = [];
    return;
  }
  for(let i = 0; i < len; i++) {
    if(!state[i]) continue;
    if(state[i].finished) continue;
    var dump = state[i];
    let frame = dump.stack[dump.stack.length - 1];
    var fnid = rules[frame.rule].value[frame.branch][frame.pos];
    var fn = rules[fnid];
    if(fn.type === "simple") {
      // Check if thread needs to be kil
      if(master.charAt(dump.pos) !== fn.value) {
        delete state[i]; // Not match, kill
        continue;
      }
      dump.pos++;
      frame.pos++;
      var loop = true;
      while(loop) {
        loop = false;
        if(rules[frame.rule].value[frame.branch].length <= frame.pos) {
          dump.stack.pop(); // Return from function
          if(dump.stack.length === 0) {
            // fn 0 finished
            if(dump.pos !== master.length) {
              delete state[i];
            } else {
              state[i].finished = true;
            }
          } else {
            frame = dump.stack[dump.stack.length - 1];
            frame.pos++;
            loop = true;
          }
        }
      }
    } else if(fn.type === "complex") {
      // New stack frame
      if(fn.value.length === 2) {
        state.push({
          pos: dump.pos,
          stack: dump.stack.concat([{ rule: fnid, pos: 0, branch: 1 }])
        });
      }
      dump.stack.push({ rule: fnid, pos: 0, branch: 0 });
    }
  }
}

function allFinished() {
  return state.filter(v => v).every(v => v.finished);
}

function removeReferenceShit() {
  state = JSON.parse(JSON.stringify(state));
}

function matchStr(str) {
  state = [{
    pos: 0,
    stack: [{ rule: 0, pos: 0, branch: 0 }]
  }];
  master = str;
  while(!allFinished()) {
    digestSymbol();
    // All the hydra action in digestSymbol creates a lot of holes
    // fix em
    // actually fuck that ther'es something wrong and i need the indiceds not to change
    //state = state.filter(v => v);
    removeReferenceShit();
  }
  return Array.from(state).filter(v => v);
}

var cnt = 0;
for(var i = 0; i < cases.length; i++) {
  if(matchStr(cases[i]).length) cnt++;
}

console.log(cnt);