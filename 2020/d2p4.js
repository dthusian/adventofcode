var input = require("fs").readFileSync("input.txt").toString();
var things = input.split(/[\r\n]+/g).map(v => v.split(/[\s-:]+/gi));

console.log(things.filter(v => (v[2] === v[3][parseInt(v[0]) - 1] ? 1 : 0) + (v[2] === v[3][parseInt(v[1]) - 1] ? 1 : 0) === 1).length);