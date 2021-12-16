console.log(require("fs").readFileSync("input.txt").toString().split(/[\r\n]+/g).map(v => {
  let stack = [];
  for(let i = 0; i < v.length; i++) {
    const v2 = v[i];
    if(v2 === "(" || v2 === "[" || v2 === "{" || v2 === "<") {
      stack.push(v2);
    }
    if(v2 === ")") {
      if(stack.pop() !== "(") return 3;
    }
    if(v2 === "]") {
      if(stack.pop() !== "[") return 57;
    }
    if(v2 === "}") {
      if(stack.pop() !== "{") return 1197;
    }
    if(v2 === ">") {
      if(stack.pop() !== "<") return 25137;
    }
  };
  return 0;
}).reduce((a, b) => a + b));
