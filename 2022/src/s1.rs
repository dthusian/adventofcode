use std::fs::File;
use std::io::Read;

pub fn main() {
  let mut input = String::new();
  File::open("input.txt").unwrap().read_to_string(&mut input).unwrap();
  let mut ans: Vec<_> = input.split("\n\n")
    .map(|v| {
      v.split("\n").map(|v| v.parse::<i64>().unwrap()).reduce(|a, b| a + b).unwrap()
    })
    .collect();
  ans.sort();
  ans.reverse();
  println!("{}", ans[0] + ans[1] + ans[2]);
}