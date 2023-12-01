function vecadd(a, b) {
  return a.map((v, i) => v + b[i]);
}

function vecsub(a, b) {
  return a.map((v, i) => v - b[i]);
}

function vecmul(a, b) {
  return a.map((v, i) => v * b[i]);
}

function veceq(a, b) {
  return a.every((v, i) => v === b[i]);
}

const COMMON_BEACON = 12;
let input = [];
let rawInput = require("fs").readFileSync("input.txt").toString().split(/[\r\n]+/g);
let i = 0;
rawInput.forEach(v => {
  if(v.startsWith("---")) {
    i = parseInt(v.split(" ")[2]);
    input[i] = {
      raw: []
    };
  } else {
    input[i].raw.push(v.split(",").map(v2 => parseInt(v2)));
  }
});
input.forEach(regenDeltas);

function looseCmp(a, b) {
  const sa = a.map(Math.abs).sort((va, vb) => va - vb);
  const sb = b.map(Math.abs).sort((va, vb) => va - vb);
  return sa.every((v, i) => v === sb[i]);
}

function notSoLooseCmp(a, b) {
  const sa = a.sort((va, vb) => Math.abs(va) - Math.abs(vb));
  const sb = b.sort((va, vb) => Math.abs(va) - Math.abs(vb));
  return sa.every((v, i) => v === sb[i]) || sa.every((v, i) => v === -sb[i]);
}

function notSoLooseCmp2(a, b) {
  return looseCmp(a, b) && Math.sign(a[0]) === 1;
}

function regenDeltas(v) {
  v.deltas = [];
  for(let i = 0; i < v.raw.length; i++) {
    for(let j = 0; j < v.raw.length; j++) {
      v.deltas[i] = v.deltas[i] || [];
      v.deltas[i][j] = vecsub(v.raw[i], v.raw[j]);
    }
  }
}

function findIntersect(sa, sb) {
  let matches = [];
  for(let ai = 0; ai < sa.deltas.length; ai++) {
    for(let aj = 0; aj < sa.deltas.length; aj++) {
      for(let bi = 0; bi < sb.deltas.length; bi++) {
        for(let bj = 0; bj < sb.deltas.length; bj++) {
          if(ai !== aj && bi !== bj && looseCmp(sa.deltas[ai][aj], sb.deltas[bi][bj])) {
            matches.push([ai, aj, bi, bj]);
          }
        }
      }
    }
  }
  let takenAs = [];
  let matchingSet = [];
  matches.forEach(v => {
    if(!takenAs.includes(v[0])) {
      takenAs.push(v[0]);
      matchingSet.push([v[0], v[2]]);
    }
  });
  return { matches: matches, matchingSet: matchingSet };
}

Array.prototype.swap = function(a, b) {
  const tmp = this[a];
  this[a] = this[b];
  this[b] = tmp;
  return this;
}

function findTransformations(sa, sb, matching) {
  //for(let i = 0; i < matchingSet.length - 1; i += 2) {
    let transforms = [];
    // Swap to get into the right spot
    let from1 = sb.raw[matching[2]];
    let to1 = sa.raw[matching[0]];
    let from2 = sb.raw[matching[3]];
    let to2 = sa.raw[matching[1]];
    console.log(from1, to1, from2, to2);
    //tag 3dim
    (() => {
      let deltaa = vecsub(to1, to2).map(Math.abs);
      let deltab = vecsub(from1, from2).map(Math.abs);
      if(deltaa[0] === deltab[1]) {
        deltab.swap(0, 1);
        transforms.push(["swap", 0, 1]);
      } else if(deltaa[0] === deltab[2]) {
        deltab.swap(0, 2);
        transforms.push(["swap", 0, 2]);
      }
      if(deltaa[1] === deltab[2]) {
        deltab.swap(1, 2);
        transforms.push(["swap", 1, 2]);
      }
    })();
    from1 = applyTransformations(from1, transforms);
    from2 = applyTransformations(from2, transforms);
    (() => {
      let deltaa = vecsub(to1, to2);
      let deltab = vecsub(from1, from2);
      deltaa.forEach((_, i) => {
        if(deltab[i] * -1 === deltaa[i]) {
          from1[i] *= -1;
          from2[i] *= -1;
          transforms.push(["neg", i]);
        }
      });
    })();
    transforms.push(["trans", vecsub(to1, from1)]);
    if(!veceq(applyTransformations(from1, transforms), to1)) throw "poo";
    if(!veceq(applyTransformations(from2, transforms), to2)) throw "poo";
    return transforms;
  //}
}

function applyTransformations(coord, transforms) {
  let ncoord = Array.from(coord);
  transforms.forEach(v => {
    if(v[0] === "swap") {
      ncoord.swap(v[1], v[2]);
    } else if(v[0] === "neg") {
      ncoord[v[1]] *= -1;
    } else if(v[0] === "trans") {
      ncoord = vecadd(ncoord, v[1]);
    }
  });
  return ncoord;
}

let matches = findIntersect(input[0], input[1]);
console.log(matches.matches);
console.log(matches.matchingSet);
for(let i = 0; i < matches.matches; i++) {
  try {
    console.log(findTransformations(input[0], input[1], matches.matches[i]));
    break;
  } catch(e) { }
}
debugger;