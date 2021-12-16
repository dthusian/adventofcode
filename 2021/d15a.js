const input = require("fs").readFileSync("input.txt").toString().split(/\r\n|\n/g).map(v => v.split("").map(v2 => parseInt(v2)));

function cvt1(v) {
  return v[1] * 100 + v[0];
}

function cvt2(v) {
  return [v % 100, Math.floor(v / 100)];
}

function adj1(v, min = -Infinity, max = Infinity) {
  return [
    [v[0] - 1, v[1]],
    [v[0] + 1, v[1]],
    [v[0], v[1] - 1],
    [v[0], v[1] + 1]
  ].filter(v2 => v2[0] >= min && v2[0] < max && v2[1] >= min && v2[1] < max).map(cvt1);
}

const unvisited = (new Set((new Array(10000).fill(0).map((v, i) => i))));
const dist = Object.fromEntries(Array.from(unvisited.values()).map(v => [v, Infinity]));

dist[0] = 0;
let current = 0;
while(unvisited.size) {
  if(current === 9999) {
    break;
  }
  unvisited.delete(current);
  let near = adj1(cvt2(current), 0, 100).filter(v => unvisited.has(v)).forEach(v => {
    dist[v] = Math.min(dist[v], dist[current] + input[cvt2(v)[1]][cvt2(v)[0]]);
  });
  current = Array.from(unvisited.values()).reduce((a, b) => dist[a] < dist[b] ? a : b);
}

console.log(dist[9999]);