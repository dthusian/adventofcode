const input = require("fs").readFileSync("input.txt").toString().split(/\r*\n+/g);
let x = 0, y = 0, aim = 0;
input.forEach(v => {
  const cmd = v.split(" ");
  cmd[1] = parseInt(cmd[1]);
  switch(cmd[0]) {
    case "forward": {
      x += cmd[1];
      y += cmd[1] * aim;
      break;
    }
    case "down": {
      aim += cmd[1];
      break;
    }
    case "up": {
      aim -= cmd[1];
      break;
    }
  }
});
console.log(x * y);