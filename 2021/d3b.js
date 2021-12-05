const input = require("fs").readFileSync("input.txt").toString().split(/\r*\n+/g);
let b = (new Array(input[0].length)).fill(0);
for(let i = 0; i < input.length; i++) {
  for(let j = 0; j < input[i].length; j++) {
    b[j] += parseInt(input[i].charAt(j));
  }
}
b = b.map(v => (v >= input.length / 2) ? "1" : "0").join("");
console.log(b);
let o2rate = Array.from(input), co2rate = Array.from(input);
let i = 0;
while(o2rate.length > 1) {
  o2rate = o2rate.filter(v => v.charAt(i) === b.charAt(i));
  i++;
}
i = 0;
while(co2rate.length > 1) {
  co2rate = co2rate.filter(v => v.charAt(i) !== b.charAt(i));
  i++;
}
console.log(o2rate);
console.log(co2rate);
console.log(parseInt(o2rate[0], 2) * parseInt(co2rate[0], 2));