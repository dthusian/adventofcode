use std::collections::{HashMap};
use bstr::{BStr, BString, ByteSlice};
use crate::util::input;

fn parse_snafu(s: &BStr) -> i64 {
  let digits = HashMap::from([
    (b'=', -2),
    (b'-', -1),
    (b'0', 0),
    (b'1', 1),
    (b'2', 2)
  ]);
  s.iter().rev().enumerate().map(|v| 5i64.pow(v.0 as u32) * digits[v.1]).sum()
}

fn to_snafu(x: i64) -> BString {
  let digits = HashMap::from([
    (-2, b'='),
    (-1, b'-'),
    (0, b'0'),
    (1, b'1'),
    (2, b'2')
  ]);
  // little endian
  let mut base5 = vec![];
  let mut x = x;
  while x > 0 {
    base5.push(x % 5);
    x /= 5;
  }
  for i in 0..base5.len() {
    if base5[i] >= 3 {
      base5[i] -= 5;
      base5[i + 1] += 1;
    }
  }
  base5.into_iter().map(|v| digits[&v]).rev().collect::<BString>()
}

pub fn main() {
  println!("{}", to_snafu(input().split_str("\n").map(|v| parse_snafu(v.into())).sum::<i64>()));
}