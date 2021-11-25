// [y][x]
var input = require("fs").readFileSync("input.txt").toString().split("\r\n\r\n").map(v => v.split(":\r\n")).map(v => ({ id: parseInt(v[0].split("Tile ")[1]), data: v[1].split("\r\n").map(v => v.split("")) }));

function extractEdges(tile) {
  var edges = [
    tile[0],
    tile[tile.length - 1],
    tile.map(v => v[0]),
    tile.map(v => v[v.length - 1])
  ].map(v => {
    
  });
  return;
}

function count(arr) {
  var cnt = {};
  arr.forEach(v => {
    if(!cnt[v]) cnt[v] = 0;
    cnt[v]++;
  });
  return cnt;
}

var edges = input.map(v => ({ id: v.id, edges: extractEdges(v.data) }));

var edgeMap = {};
edges.forEach(v => v.edges.forEach(v2 => edgeMap[v2] ? edgeMap[v2].push(v.id) : (edgeMap[v2] = [v.id]) ));
