// thx stackoverflow
// https://stackoverflow.com/a/14438954
function onlyUnique(value, index, self) {
  return self.indexOf(value) === index;
}

const out = require("fs")
  .readFileSync("input.txt")
  .toString()
  .split(/(\r\n|\n){2}/mig)
  .map(v => v
    .split("")
    .filter(onlyUnique)
    .filter(v => !v.match(/\s/)))
  .reduce((a, v) => a + v.length, 0);

console.log(out);