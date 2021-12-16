let bonds = {};

function safeInc(obj, key, amnt = 1) {
  if(!obj[key]) obj[key] = 0;
  obj[key] += amnt;
}

let [template, rawrules] = require("fs").readFileSync("input.txt").toString().split(/\r\n\r\n|\n\n/g);
template = " " + template + " ";
for(let i = 0; i < template.length - 1; i++) {
  const bond = template.substring(i, i + 2);
  if(!bonds[bond]) bonds[bond] = 0;
  safeInc(bonds, bond);
}
const rules = Object.fromEntries(rawrules.split(/\r\n/g).map(v => v.split(" -> ")));

function step(bonds) {
  const newBonds = {};
  Object.entries(bonds).map(v => {
    if(rules[v[0]]) {
      const sub = rules[v[0]];
      safeInc(newBonds, v[0][0] + sub, v[1]);
      safeInc(newBonds, sub + v[0][1], v[1]);
    } else {
      safeInc(newBonds, v[0], v[1]);
    }
  });
  return newBonds;
}

function boil(bonds) {
  const elCnts = {};
  Object.entries(bonds).map(v => {
    safeInc(elCnts, v[0][0], v[1]);
    safeInc(elCnts, v[0][1], v[1]);
  });
  return Object.fromEntries(Object.entries(elCnts).filter(v => v[0] !== ' ').map(v => [v[0], v[1] / 2]));
}

for(let i = 0; i < 40; i++) bonds = step(bonds);
const boiled = boil(bonds);
const min = Object.values(boiled).sort((a, b) => a - b)[0];
const max = Object.values(boiled).sort((a, b) => b - a)[0];
console.log(max - min);