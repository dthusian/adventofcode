use std::collections::VecDeque;
use std::fs::File;
use std::io::Read;
use std::str::FromStr;
use bstr::{BString, ByteSlice};

macro_rules! u8s {
  ($s:expr) => { String::from_utf8_lossy($s) }
}

pub fn main() {
  let mut input = BString::default();
  File::open("input.txt").unwrap().read_to_end(&mut input).unwrap();
  let (stacks0, instrs0) = input.split_once_str("\n\n").unwrap();
  let stacks0 = stacks0.split_str("\n").collect::<Vec<&[u8]>>();
  let mut stacks1 = vec![];
  for i in 0..9 {
    stacks1.push(vec![]);
    for j in 0..8 {
      stacks1[i].push(u8::from(stacks0[7 - j][i * 4 + 1]));
    }
  }
  let mut stacks2 = stacks1.into_iter()
    .map(|v| v.into_iter().filter(|v2| *v2 != b' ').collect::<Vec<_>>())
    .map(|v| VecDeque::from(v))
    .collect::<Vec<VecDeque<_>>>();
  let instrs1 = instrs0.split_str("\n")
    .into_iter()
    .map(|v| v.split_str(" ").collect::<Vec<&[u8]>>())
    .map(|v| (usize::from_str(&u8s!(v[1])).unwrap(), usize::from_str(&u8s!(v[3])).unwrap() - 1, usize::from_str(&u8s!(v[5])).unwrap() - 1))
    .collect::<Vec<_>>();
  for (count, from, to) in instrs1 {
    let mut v = vec![];
    for _ in 0..count {
      v.push(stacks2[from].pop_back().unwrap());
    }
    v.reverse();
    for v2 in v {
      stacks2[to].push_back(v2);
    }
  }
  let word = stacks2.iter()
    .map(|v| v.back())
    .map(|v| match v { Some(v2) => *v2, None => b'#'})
    .collect::<Vec<_>>();
  println!("{}", u8s!(&word));
}