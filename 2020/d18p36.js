// They're all 1-digit numbers
function lexExpr(str) {
  return str.replace(/\s/ig, "").split("");
}

function evalExpr(expr) {
  var nexpr = Array.from(expr);
  // Pass 0: Evaluate paren'd expressions
  for(var i = 0; i < nexpr.length; i++) {
    if(nexpr[i] === "(") {
      var depth = 1;
      var buf = [];
      var start = i;
      var dcount = 1;
      while(depth > 0) {
        i++;
        dcount++;
        buf.push(nexpr[i]);
        if(nexpr[i] === "(") depth++;
        if(nexpr[i] === ")") depth--;
      }
      buf.pop();
      nexpr.splice(start, dcount, evalExpr(buf));
      i = start + 1;
    }
  }
  // Pass 1: Compute addition
  for(var i = 0; i < nexpr.length; i++) {
    if(nexpr[i] === "+") {
      var result = parseInt(nexpr[i - 1]) + parseInt(nexpr[i + 1]);
      nexpr.splice(i - 1, 3, result);
      i -= 2;
    }
  }
  // Pass 1: Compute multiplication
  for(var i = 0; i < nexpr.length; i++) {
    if(nexpr[i] === "*") {
      var result = parseInt(nexpr[i - 1]) * parseInt(nexpr[i + 1]);
      nexpr.splice(i - 1, 3, result);
      i -= 2;
    }
  }
  return parseInt(nexpr[0]);
}

console.log(require("fs").readFileSync("input.txt").toString().split("\r\n").map(lexExpr).map(evalExpr).reduce((a, b) => a + b));