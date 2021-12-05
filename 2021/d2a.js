const input = require("fs").readFileSync("input.txt").toString().split(/\r*\n+/g);
let x = 0, y = 0;
input.forEach(v => {
  const cmd = v.split(" ");
  cmd[1] = parseInt(cmd[1]);
  switch(cmd[0]) {
    case "forward": {
      x += cmd[1];
      break;
    }
    case "down": {
      y += cmd[1];
      break;
    }
    case "up": {
      y -= cmd[1];
      break;
    }
  }
});
console.log(x * y);