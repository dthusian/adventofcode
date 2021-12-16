const adj = {};

require("fs").readFileSync("input.txt").toString().split(/[\r\n]+/g).map(v => v.split("-")).forEach(v => {
  if(!adj[v[0]]) {
    adj[v[0]] = [];
  }
  if(!adj[v[1]]) {
    adj[v[1]] = [];
  }
  adj[v[0]].push(v[1]);
  adj[v[1]].push(v[0]);
});

function recursiveSearch(node, visited, visitedSmall2) {
  if(node === "end") {
    return 1;
  }
  let newVisited;
  if(node.toUpperCase() === node) {
    newVisited = visited;
  } else {
    newVisited = visited.concat([node]);
  }
  return adj[node].map(v => {
    if(visited.includes(v)) {
      if(!visitedSmall2 && v !== "start") {
        return recursiveSearch(v, newVisited, true);
      } else {
        return 0;
      }
    }
    return recursiveSearch(v, newVisited, visitedSmall2);
  }).reduce((a, b) => a + b);
}

console.log(recursiveSearch("start", [], false));