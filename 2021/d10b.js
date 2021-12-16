console.log(require("fs").readFileSync("input.txt").toString().split(/[\r\n]+/g).map(v => {
  let total = 0;
  let stack = [];
  for(let i = 0; i < v.length; i++) {
    const v2 = v[i];
    if(v2 === "(" || v2 === "[" || v2 === "{" || v2 === "<") {
      stack.push(v2);
    }
    if(v2 === ")") {
      if(stack.pop() !== "(") return 0;
    }
    if(v2 === "]") {
      if(stack.pop() !== "[") return 0;
    }
    if(v2 === "}") {
      if(stack.pop() !== "{") return 0;
    }
    if(v2 === ">") {
      if(stack.pop() !== "<") return 0;
    }
  };
  stack.reverse().forEach(v => {
    total *= 5;
    total += ["(", "[", "{", "<"].indexOf(v) + 1;
  });
  return total;
}).filter(v => v).sort((a, b) => a - b).filter((v, i, l) => i === Math.floor(l.length / 2))[0]
);
