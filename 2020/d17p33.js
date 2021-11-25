var raw = require("fs").readFileSync("input.txt").toString().split("\r\n").map(v => v.split(""));
var imat3 = (new Array(raw[0].length)).fill(0).map((v, i) => raw.map(v => [v[i]]));

// all mat3 are accessed like [x][y][z]

function createMat3(size) {
  var d1 = ".".repeat(size[2]).split("");
  var d2 = (new Array(size[1])).fill(0).map(v => Array.from(d1));
  var d3 = (new Array(size[0])).fill(0).map(v => JSON.parse(JSON.stringify(d2)));
  return d3;
}

function copyMat3(dst, src, coord) {
  for(var x = 0; x < src.length; x++) {
    for(var y = 0; y < src[x].length; y++) {
      for(var z = 0; z < src[x][y].length; z++) {
        var cx = x + coord[0];
        var cy = y + coord[1];
        var cz = z + coord[2];
        dst[cx][cy][cz] = src[x][y][z];
      }
    }
  }
}

function visualiseMat3(mat) {
  for(var z = 0; z < mat[0][0].length; z++) {
    console.log("z=" + z);
    for(var y = 0; y < mat[0].length; y++) {
      var buf = "";
      for(var x = 0; x < mat.length; x++) {
        buf += mat[x][y][z];
      }
      console.log(buf);
    }
  }
}

function getMat3Size(mat) {
  return [mat.length, mat[0].length, mat[0][0].length];
}

function enumerateNeighbors(coord) {
  var buf = [];
  for(var x = 0; x < 3; x++) {
    for(var y = 0; y < 3; y++) {
      for(var z = 0; z < 3; z++) {
        if(x === 1 && y === 1 && z === 1) continue;
        buf.push([coord[0] + x - 1, coord[1] + y - 1, coord[2] + z - 1]);
      }
    }
  }
  return buf;
}

function inRange(range, value) {
  return value >= range[0] && value < range[1];
}

function popcnt(mat, coord) {
  var size = getMat3Size(mat);
  var tmp =
   enumerateNeighbors(coord);
  tmp = tmp.filter(v => v.every((_, i) => inRange([0, size[i]], v[i])));
  tmp = tmp.map(v => mat[v[0]][v[1]][v[2]]);
  tmp = tmp.filter(v => v === "#").length;
  return tmp;
}

function popcntall(mat) {
  return mat.flat(Infinity).filter(v => v === "#").length;
}

var sim = createMat3([6 + 8 + 6, 6 + 8 + 6, 6 + 1 + 6]);

function stepSim() {
  var size = getMat3Size(sim);
  var newsim = createMat3(size);
  for(var x = 0; x < size[0]; x++) {
    for(var y = 0; y < size[1]; y++) {
      for(var z = 0; z < size[2]; z++) {
        var cnt = popcnt(sim, [x, y, z]);
        var ccell = sim[x][y][z];
        if(ccell === "#" && cnt !== 2 && cnt !== 3) {
          newsim[x][y][z] = ".";
        }else if(ccell === "." && cnt === 3) {
          newsim[x][y][z] = "#";
        }else{
          newsim[x][y][z] = ccell;
        }
      }
    }
  }
  sim = newsim;
}

copyMat3(sim, imat3, [6, 6, 6]);

for(var i = 0; i < 6; i++) stepSim();

console.log(popcntall(sim));