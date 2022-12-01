use std::fs::File;
use std::io::Read;

pub fn main() {
  let mut input = String::new();
  File::open("input.txt").unwrap().read_to_string(&mut input).unwrap();
  let ans = input.split("\n\n")
    .map(|v| {
      v.split("\n").map(|v| v.parse::<i64>().unwrap()).reduce(|a, b| a + b).unwrap()
    })
    .reduce(|a, b| a.max(b))
    .unwrap();
  println!("{}", ans);
}