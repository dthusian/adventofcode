use std::cmp::max;
use std::fs::File;
use std::io::Read;
use bstr::BString;

pub fn main() {
  let mut input = BString::default();
  File::open("input.txt").unwrap().read_to_end(&mut input).unwrap();
  let mut cnt = [0u8; 26];
  let mut n2 = 0;
  for i in 0..input.len() {
    let chr = input[i];
    let idx = (chr - b'a') as usize;
    cnt[idx] += 1;
    if cnt[idx] == 2 {
      n2 += 1;
    }
    if i >= 14 {
      let chr = input[i - 14];
      let idx = (chr - b'a') as usize;
      cnt[idx] -= 1;
      if cnt[idx] == 1 {
        n2 -= 1;
      }
      if n2 == 0 {
        println!("{}", i + 1);
        break;
      }
    }
  }
}