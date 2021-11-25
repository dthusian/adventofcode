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
  var fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
  var missingFields = fields.filter(f => !o[f]);
  if(missingFields.length === 0) return true;
  return false;
}

function hasValidFields(o) {
  // Year validation
  var byr = parseInt(o.byr);
  var iyr = parseInt(o.iyr);
  var eyr = parseInt(o.eyr);
  if(isNaN(byr) || byr < 1920 || byr > 2002) return false;
  if(isNaN(iyr) || iyr < 2010 || iyr > 2020) return false;
  if(isNaN(eyr) || eyr < 2020 || eyr > 2030) return false;
  // Regex validation
  if(!/^#[0-9a-f]{6}$/.test(o.hcl)) return false;
  if(!/^[0-9]{9}$/.test(o.pid)) return false;
  // Eyecol
  if("amb blu brn gry grn hzl oth".split(" ").indexOf(o.ecl) === -1) return false;
  // Height
  var hgtunit = o.hgt.substring(o.hgt.length - 2);
  var hgt = parseInt(o.hgt.substring(0, o.hgt.length - 2));
  if(isNaN(hgt)) return false;
  if(hgtunit === "in") {
    if(hgt < 59 || hgt > 76) return false;
  } else if(hgtunit === "cm") {
    if(hgt < 150 || hgt > 193) return false;
  } else return false;
  return true;
}

console.log(parsedPassports.filter(hasFields).filter(hasValidFields).length);