const input = require("fs").readFileSync("input.txt").toString().split(/\r*\n+/g);

function findCommon(arr, pos) {
  const cnt = arr.map(v => parseInt(v[pos])).reduce((a, b) => a + b);
  return (cnt >= arr.length / 2) ? "1" : "0";
}

let o2rate = Array.from(input), co2rate = Array.from(input);
let i = 0;
while(o2rate.length > 1) {
  const common = findCommon(o2rate, i);
  o2rate = o2rate.filter(v => v.charAt(i) === common);
  i++;
}
i = 0;
while(co2rate.length > 1) {
  const common = findCommon(co2rate, i);
  co2rate = co2rate.filter(v => v.charAt(i) !== common);
  i++;
}
console.log(o2rate);
console.log(co2rate);
console.log(parseInt(o2rate[0], 2) * parseInt(co2rate[0], 2));