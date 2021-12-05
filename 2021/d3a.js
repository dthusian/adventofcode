const input = require("fs").readFileSync("input.txt").toString().split(/\r*\n+/g);
let b = (new Array(input[0].length)).fill(0);
for(let i = 0; i < input.length; i++) {
  for(let j = 0; j < input[i].length; j++) {
    b[j] += parseInt(input[i].charAt(j));
  }
}
console.log(b);
b = b.reverse();
let gamma = 0;
let epsilon = 0;
for(let i = 0; i < input[0].length; i++) {
  if(b[i] < input.length / 2) {
    epsilon |= 1 << i;
  } else {
    gamma |= 1 << i;
  }
}
console.log(gamma * epsilon);