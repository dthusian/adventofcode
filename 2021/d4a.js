const input = require("fs").readFileSync("input.txt").toString().split(/(\r\n|\n){2,}/g).filter(v => !/^\n+$/g.test(v));

const stream = input[0].split(",").map(v => parseInt(v));
const boards = input.slice(1).map(v => v.split(/(\r\n|\n)/g).filter(v => !/^\n+$/g.test(v)).map(v2 => v2.split(/\s+/g).filter(v => v.trim().length > 0).map(v3 => parseInt(v3))));

function isWinner(board) {
  for(let i = 0; i < board.length; i++) {
    if(board[i].every(v => v === true)) {
      return true;
    }
  }
  for(let i = 0; i < board[0].length; i++) {
    if(board.map(v => v[i]).every(v => v === true)) {
      return true;
    }
  }
  return false;
}

for(let i = 0; i < stream.length; i++) {
  const nextInt = stream[i];
  for(let j = 0; j < boards.length; j++) {
    boards[j] = boards[j].map(v => v.map(v2 => v2 === nextInt ? true : v2));
    if(isWinner(boards[j])) {
      let sum = boards[j].flat().filter(v => typeof v === "number").reduce((a, b) => a + b);
      console.log(sum * nextInt);
      process.exit(0);
    }
  }
}