var input = require("fs").readFileSync("input.txt").toString().split("\r\n").map(v => [v[0], parseInt(v.slice(1))]);

// I am not reimplementing SSE.
// I am not reimplementing SSE.
// I am not reimplementing SSE.
// I am not reimplementing SSE.
// I am not reimplementing SSE.

function vaddpd(a, b) {
  return [a[0] + b[0], a[1] + b[1]];
}

function vsubpd(a, b) {
  return [a[0] - b[0], a[1] - b[1]];
}

function vmulpd(a, b) {
  return [a[0] * b[0], a[1] * b[1]];
}

function vhaddpd(x) {
  return x[0] + x[1];
}

function vabspd(x) {
  return [Math.abs(x[0]), Math.abs(x[1])];
}

function vbroadcastpd(x) {
  return [x, x];
}

function rep(fn, j) {
  for(var i = 0; i < j; i++) fn(i);
}

var wpos = [10, -1];
var pos = [0, 0];

function rotl(v) {
  return [v[1], -v[0]];
}

function rotr(v) {
  return [-v[1], v[0]];
}

input.forEach((v) => {
  var x = v[1];
  var vecx = vbroadcastpd(x);
  switch(v[0]) {
    case "N": {
      wpos = vaddpd(wpos, vmulpd(vecx, [0, -1]));
      break;
    }
    case "E": {
      wpos = vaddpd(wpos, vmulpd(vecx, [1, 0]));
      break;
    }
    case "S": {
      wpos = vaddpd(wpos, vmulpd(vecx, [0, 1]));
      break;
    }
    case "W": {
      wpos = vaddpd(wpos, vmulpd(vecx, [-1, 0]));
      break;
    }
    case "L": {
      rep(v => { wpos = vaddpd(pos, rotl(vsubpd(wpos, pos))) }, v[1] / 90);
      break;
    }
    case "R": {
      rep(v => { wpos = vaddpd(pos, rotr(vsubpd(wpos, pos))) }, v[1] / 90);
      break;
    }
    case "F": {
      var vel = vmulpd(vsubpd(wpos, pos), vecx);
      pos = vaddpd(pos, vel);
      wpos = vaddpd(wpos, vel);
      break;
    }
  }
});

console.log(vhaddpd(vabspd(pos)));