const input = require("fs").readFileSync("input.txt").toString().split(/\r\n|\n/g).map(v => v.split("").map(v2 => parseInt(v2)));

function incmap(v, x) {
  return v.map(v2 => v2.map(v3 => {
    const n = v3 + x;
    if(n > 9) return n - 9;
    return n;
  }));
}

function incarr(v, x) {
  return v.map(v2 => {
    const n = v2 + x;
    if(n > 9) return n - 9;
    return n;
  });
}

let fullmap = (input.concat(incmap(input, 1)).concat(incmap(input, 2)).concat(incmap(input, 3)).concat(incmap(input, 4)));

fullmap = fullmap.map(v => v.concat(incarr(v, 1)).concat(incarr(v, 2)).concat(incarr(v, 3)).concat(incarr(v, 4)));

function cvt1(v) {
  return v[1] * 500 + v[0];
}

function cvt2(v) {
  return [v % 500, Math.floor(v / 500)];
}

function adj1(v, min = -Infinity, max = Infinity) {
  return [
    [v[0] - 1, v[1]],
    [v[0] + 1, v[1]],
    [v[0], v[1] - 1],
    [v[0], v[1] + 1]
  ].filter(v2 => v2[0] >= min && v2[0] < max && v2[1] >= min && v2[1] < max).map(cvt1);
}

const unvisited = (new Set((new Array(250000).fill(0).map((v, i) => i))));
const dist = Object.fromEntries(Array.from(unvisited.values()).map(v => [v, Infinity]));

const end = 499 * 500 + 499;

dist[0] = 0;
let current = 0;
while(unvisited.size) {
  if(current === end) {
    break;
  }
  unvisited.delete(current);
  adj1(cvt2(current), 0, 500).filter(v => unvisited.has(v)).forEach(v => {
    dist[v] = Math.min(dist[v], dist[current] + fullmap[cvt2(v)[1]][cvt2(v)[0]]);
  });
  current = Array.from(unvisited.values()).reduce((a, b) => dist[a] < dist[b] ? a : b);
}

console.log(dist[end]);