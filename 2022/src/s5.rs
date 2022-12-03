
use std::collections::BTreeSet;
use std::fs::File;
use std::io::Read;
use bstr::{BString, ByteSlice};

pub fn main() {
  let mut input = BString::default();
  File::open("input.txt").unwrap().read_to_end(&mut input).unwrap();
  let ans = input.split_str("\n")
    .array_chunks::<3>()
    .map(|v| (
      BTreeSet::from_iter(v[0].iter()),
      BTreeSet::from_iter(v[1].iter()),
      BTreeSet::from_iter(v[2].iter()),
    ))
    .map(|v| v.0
      .intersection(&v.1)
      .map(|v| *v).collect::<BTreeSet<&u8>>()
      .intersection(&v.2)
      .map(|v| *v).collect::<BTreeSet<&u8>>()
      .iter().map(|v| **v).collect::<Vec<u8>>())
    .map(|v| v[0])
    .map(|v| if v <= 0x5a { v - 0x41 + 27 } else if v <= 0x7a { v - 0x61 + 1 } else { 0 })
    .map(|v| v as i64)
    .sum::<i64>();
  println!("{}", ans);
}