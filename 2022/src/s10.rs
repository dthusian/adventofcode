use std::fs::File;
use std::io::Read;
use bstr::BString;

pub fn main() {
  let mut input = BString::default();
  File::open("input.txt").unwrap().read_to_end(&mut input).unwrap();
  for i in 0..input.len() - 4 {
    if input[i] == input[i + 1] || input[i + 2] == input[i + 3] ||
      input[i] == input[i + 2] || input[i + 1] == input[i + 3] ||
      input[i] == input[i + 3] || input[i + 1] == input[i + 2] {
      continue;
    } else {
      println!("{}", i + 4);
      break;
    }
  }
}