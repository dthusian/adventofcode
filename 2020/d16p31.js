// Complex input
var raw = require("fs").readFileSync("input.txt").toString().split("\r\n\r\n");

var fields = {};
raw[0].split("\r\n").forEach(v => {
  var tmp0 = v.split(": ");
  var key = tmp0[0];
  var tmp1 = tmp0[1].split(" or ");
  var tmp2 = tmp1.map(v => v.split("-").map(v => parseInt(v)));
  fields[key] = tmp2;
});
var myticket = raw[1].split("\r\n")[1].split(",").map(v => parseInt(v));
var nearbyTickets = raw[2].split("\r\n").slice(1).map(v => v.split(",").map(v => parseInt(v)));

var totalRangeSet = Object.values(fields).flat(1);

function isInRangeset(rangeset, value) {
  var ret = false;
  for(var i = 0; i < rangeset.length; i++) {
    var min = Math.min(rangeset[i][0], rangeset[i][1]);
    var max = Math.max(rangeset[i][0], rangeset[i][1]);
    if(value <= max && value >= min) {
      ret = true;
      break;
    }
  }
  return ret;
}

console.log(nearbyTickets.flat().map(v => isInRangeset(totalRangeSet, v) ? 0 : v).reduce((a, b) => a + b));