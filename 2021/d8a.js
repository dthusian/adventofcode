const input = require("fs").readFileSync("input.txt")
  .toString().split(/(\n|\r\n)+/g)
  .filter(v => v.trim().length)
  .map(v => v.split(" | "))
  .map(v => ({ p: v[0].split(" "), d: v[1].split(" ") }));

console.log(input.map(v => v.d).map(v => v.filter(v2 => v2.length === 2 || v2.length === 4 || v2.length === 3 || v2.length === 7)).flat().length);