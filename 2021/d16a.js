const input = require("fs").readFileSync("input.txt").toString().split("").map(v => parseInt(v, 16).toString(2).padStart(4, '0')).join("");

function readpacket(packet) {
  const obj = {};
  obj.version = parseInt(packet.substring(0, 3), 2);
  obj.type = parseInt(packet.substring(3, 6), 2);
  if(obj.type === 4) {
    let i = 6;
    let n = "";
    while(packet[i] === "1") {
      n += packet.substring(i + 1, i + 5);
      i += 5;
    }
    n += packet.substring(i + 1, i + 5);
    i += 5;
    obj.value = parseInt(n, 2);
    obj.length = i;
  } else {
    const cnttype = packet[6];
    obj.children = [];
    if(cnttype === "0") {
      const length = parseInt(packet.substring(7, 7 + 15), 2);
      let i;
      for(i = 7 + 15; i < length + 7 + 15;) {
        const inner = readpacket(packet.substring(i));
        obj.children.push(inner);
        i += inner.length;
      }
      obj.length = i;
    } else {
      const length = parseInt(packet.substring(7, 7 + 11), 2);
      let i = 7 + 11;
      for(let j = 0; j < length; j++) {
        const inner = readpacket(packet.substring(i));
        obj.children.push(inner);
        i += inner.length;
      }
      obj.length = i;
    }
  }
  return obj;
}

function part1(packet) {
  return packet.version + (packet.children ? packet.children.map(part1).reduce((a, b) => a + b, 0) : 0);
}

const packet = readpacket(input);

console.log(part1(packet));