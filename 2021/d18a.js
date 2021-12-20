const input = require("fs").readFileSync("input.txt").toString().split(/[\r\n]+/g).map(v => JSON.parse(v.trim()));

Array.prototype.last = function() {
  return this[this.length - 1];
}

Array.prototype.setlast = function (v) {
  this[this.length - 1] = v;
}

function add(a, b) {
  return [a, b];
}

function traverseGet(x, path) {
  let ptr = x;
  for(let i = 0; i < path.length; i++) ptr = ptr[path[i]];
  return ptr;
}

function traverseSet(x, path, v) {
  let ptr = x;
  for(let i = 0; i < path.length - 1; i++) ptr = ptr[path[i]];
  ptr[path[path.length - 1]] = v;
}

function backward(x, ptr) {
  while(true) {
    if(ptr.last() === 1) {
      ptr.setlast(0);
      break;
    } else {
      ptr.pop();
      if(ptr.length === 0) {
        return;
      }
    }
  }
  while(typeof traverseGet(x, ptr) === "object") {
    ptr.push(1);
  }
}

function forward(x, ptr) {
  while(true) {
    if(ptr.last() === 0) {
      ptr.setlast(1);
      break;
    } else {
      ptr.pop();
      if(ptr.length === 0) {
        return;
      }
    }
  }
  while(typeof traverseGet(x, ptr) === "object") {
    ptr.push(0);
  }
}

function explode2(x, path) {
  const pair = traverseGet(x, path);
  if(pair instanceof Array) {
    explode2(x, path.concat([0]));
    explode2(x, path.concat([1]));
    let ptr = Array.from(path);
    // Backward
    backward(x, ptr);
    if(ptr.length !== 0) {
      traverseSet(x, ptr, traverseGet(x, ptr) + pair[0]);
    }
    ptr = Array.from(path);
    // Forward
    forward(x, ptr);
    if(ptr.length !== 0) {
      traverseSet(x, ptr, traverseGet(x, ptr) + pair[1]);
    }
    traverseSet(x, path, 0);
  }
}

function split(x, path) {
  const pp = traverseGet(x, path);
  traverseSet(x, path, [Math.floor(pp / 2), Math.ceil(pp / 2)]);
}

function reduceExplode(x) {
  let ptr = [];
  while(traverseGet(x, ptr) instanceof Array) ptr.push(0);
  while(true) {
    if(ptr.length >= 5) {
      while(ptr.length >= 5) ptr.pop();
      explode2(x, ptr);
      break;
    }
    forward(x, ptr);
    if(ptr.length === 0) break;
  }
}

function reduceSplit(x) {
  let ptr = [];
  while(traverseGet(x, ptr) instanceof Array) ptr.push(0);
  while(true) {
    if(traverseGet(x, ptr) >= 10) {
      split(x, ptr);
      break;
    }
    forward(x, ptr);
    if(ptr.length === 0) break;
  }
}

function clone(x) {
  return JSON.parse(JSON.stringify(x));
}

function reduceAll(x) {
  let last2 = null;
  while(JSON.stringify(last2) !== JSON.stringify(x)) {
    last2 = clone(x);
    let last = clone(x);
    reduceExplode(x);
    if(JSON.stringify(last) === JSON.stringify(x)) {
      reduceSplit(x);
    }
  }
}

function magnitude(x) {
  if(typeof x === "number") return x;
  return magnitude(x[0]) * 3 + magnitude(x[1]) * 2;
}

const v = input.reduce((a, b) => {
  const n = add(a, b);
  reduceAll(n);
  return n;
});

reduceAll(v);

console.log(magnitude(v));