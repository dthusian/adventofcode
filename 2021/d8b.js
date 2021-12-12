const input = require("fs").readFileSync("input.txt")
  .toString().split(/(\n|\r\n)+/g)
  .filter(v => v.trim().length)
  .map(v => v.split(" | "))
  .map(v => ({ p: v[0].split(" "), d: v[1].split(" ") }));

function filterPoo(v, i /* wire_id[] */, x /* segment_id[] */) {
  return v.map((v2, i2) => v2.split("").filter(v3 =>
    i.includes(i2) === x.includes(v3) // XNOR
  ).join(""));
}

function iWantToBangGanyu(v, i /* wire_id[] */, x /* segment_id[] */) {
  return v.map((v2, i2) => v2.split("").filter(v3 =>
    i.includes(i2) ? x.includes(v3) : true
  ).join(""));
}

function deduce1(v) {
  // segs[wire_id] => possible_segments
  let segs = (new Array(7)).fill("abcdefg");
  v.p.forEach(v2 => {
    switch(v2.length) {
      case 2: {
        segs = filterPoo(segs, [2, 5], v2);
        break;
      }
      case 3: {
        segs = filterPoo(segs, [0, 2, 5], v2);
        break;
      }
      case 4: {
        segs = filterPoo(segs, [1, 2, 3, 5], v2);
        break;
      }
      case 5: {
        segs = iWantToBangGanyu(segs, [0, 3, 6], v2);
        break;
      }
      case 6: {
        segs = iWantToBangGanyu(segs, [0, 1, 5, 6], v2);
        break;
      }
    }
  });
  const isFinal = segs.filter(v => v.length === 1).join("");
  segs = segs.map(v2 => {
    if(v2.length === 1) return v2;
    return v2.split("").filter(v3 => !isFinal.includes(v3)).join("");
  });
  return segs.join("");
}

function transform1(v, m) {
  const wires = v.split("").map(v2 => m.indexOf(v2)).sort().map(v => v.toString()).join("");
  switch(wires) {
    case "012456": return 0;
    case "25": return 1;
    case "02346": return 2;
    case "02356": return 3;
    case "1235": return 4;
    case "01356": return 5;
    case "013456": return 6;
    case "025": return 7;
    case "0123456": return 8;
    case "012356": return 9;
    default: throw "what";
  }
}

function cvt1(v) {
  const m = deduce1(v);
  const dig = v.d.map(v2 => transform1(v2, m));
  return parseInt(dig.map(v => v.toString()).join(""));
}

console.log(input.map(cvt1).reduce((a, b) => a + b));