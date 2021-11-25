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
var totalRangeSet = Object.values(fields).flat(1);

var myticket = raw[1].split("\r\n")[1].split(",").map(v => parseInt(v));
var nearbyTickets = raw[2].split("\r\n").slice(1).map(v => v.split(",").map(v => parseInt(v))).filter(v => v.every(v => isInRangeset(totalRangeSet, v)));

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

function checkFields(fieldsCheck, value) {
  var fieldArray = Array.from(fieldsCheck.values());
  for(var i = 0; i < fieldArray.length; i++) {
    var cfield = fieldArray[i];
    if(!isInRangeset(fields[cfield], value)) {
      fieldsCheck.delete(cfield);
    }
  }
}

function removeField(fieldSetList, field) {
  for(var i = 0; i < fieldSetList.length; i++) {
    if(fieldSetList[i] instanceof Set)
      fieldSetList[i].delete(field);
  }
}

function checkSumSetList(set) {
  return set.map(v => v instanceof Set ? Array.from(v.values()).join(" ") : ["str:" + v]).join("\n");
}

var possibleFields = [];

// Pass 0: Consider each field as possible
for(var i = 0; i < nearbyTickets[0].length; i++) {
  possibleFields.push(new Set(Object.keys(fields)));
}

// Pass 1: Remove fields whose range doesn't match up
for(var i = 0; i < nearbyTickets[0].length; i++) {
  for(var j = 0; j < nearbyTickets.length; j++) {
    checkFields(possibleFields[i], nearbyTickets[j][i]);
  }
}

// Pass 2: Remove fields that are definitely already another column
var lastCheckSum = checkSumSetList(possibleFields);
while(true) {
  for(var i = 0; i < nearbyTickets[0].length; i++) {
    var sz = possibleFields[i].size;
    if(sz === 0) {
      throw new Error("what");
    }
    if(sz === 1) {
      var field = possibleFields[i].values().next().value;
      removeField(possibleFields, field);
      possibleFields[i] = field;
    }
  }
  var newCheckSum = checkSumSetList(possibleFields);
  if(newCheckSum === lastCheckSum) {
    break;
  }
  lastCheckSum = newCheckSum;
}

// Now retrieve the departure info
var acc = 1;
for(var i = 0; i < possibleFields.length; i++) {
  if(possibleFields[i].startsWith("departure")) {
    acc *= myticket[i];
  }
}

console.log(acc);