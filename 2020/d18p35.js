// They're all 1-digit numbers
function lexExpr(str) {
  return str.replace(/\s/ig, "").split("");
}

function evalExpr(expr) {
  expr = ["+"].concat(expr);
  var acc = 0;
  for(var ptr = 0; ptr < expr.length; ptr++) {
    var oper = expr[ptr];
    ptr++;
    var arg = expr[ptr];
    var val;
    if(arg === "(") {
      // Read entire expression
      var depth = 1;
      var buf = [];
      while(depth > 0) {
        ptr++;
        sym = expr[ptr];
        buf.push(sym);
        if(sym === "(") depth++;
        if(sym === ")") depth--;
      }
      buf.pop(); // Remove last unmatched paren
      val = evalExpr(buf);
    } else {
      val = parseInt(arg);
    }
    if(oper === "+") {
      acc += val;
    } else if (oper === "*") {
      acc *= val;
    }
  }
  return acc;
}

console.log(require("fs").readFileSync("input.txt").toString().split("\r\n").map(lexExpr).map(evalExpr).reduce((a, b) => a + b));