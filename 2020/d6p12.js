// thx MDN
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set
function intersection(setA, setB) {
  let _intersection = new Set()
  for (let elem of setB) {
      if (setA.has(elem)) {
          _intersection.add(elem)
      }
  }
  return _intersection
}

const groups = require("fs")
  .readFileSync("input.txt")
  .toString()
  .split(/(\r\n|\n){2}/mig);

const gsets = groups
  .map(v => v.split(/\r\n/ig).map(v => new Set(v.split(""))));

const ginters = gsets
  .map(v => v.reduce((a, v) => intersection(a, v)));

const gcounts = ginters
  .map(v => v.size);

console.log(gcounts.reduce((a, v) => a + v));