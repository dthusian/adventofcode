const input = require("fs").readFileSync("input.txt").toString();

const passports = input.split(/(\r\n|\n){2}/ig);

const parsedPassports = passports.map(v => {
  var o = {};
  v.split(/\s/ig).map(v => {
    var p = v.split(":");
    o[p[0]] = p[1];
  });
  return o;
});

function hasFields(o) {
  var fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"];
  var missingFields = fields.filter(f => !o[f]);
  if(missingFields.length === 0) return true;
  if(missingFields.length === 1 && missingFields[0] === "cid") return true;
  return false;
}

console.log(parsedPassports.filter(hasFields).length);