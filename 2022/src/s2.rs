use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use bstr::{BString, ByteSlice, ByteVec};

pub fn main() {
  let mut input = BString::default();
  File::open("input.txt").unwrap().read_to_end(input.as_vec_mut()).unwrap();
  let score: i64 = input.split_str("\n")
    .map(|v| v.split_once_str(" ").unwrap())
    .map(|v| (*v.0.get(0usize).unwrap(), *v.1.get(0usize).unwrap()))
    .map(|v| {
      let lut = HashMap::from([
        ((b'A', b'X'), 3+1),
        ((b'B', b'X'), 0+1),
        ((b'C', b'X'), 6+1),
        ((b'A', b'Y'), 6+2),
        ((b'B', b'Y'), 3+2),
        ((b'C', b'Y'), 0+2),
        ((b'A', b'Z'), 0+3),
        ((b'B', b'Z'), 6+3),
        ((b'C', b'Z'), 3+3)
      ]);
      *lut.get(&v).unwrap()
    })
    .sum();
  println!("{}", score);
}