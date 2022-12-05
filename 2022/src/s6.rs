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
  let ans: i64 = input.split_str("\n")
    .map(|v| v.split_once_str(",").unwrap())
    .map(|v| (v.0.split_once_str("-").unwrap(), v.1.split_once_str("-").unwrap()))
    .map(|v| (i64::from_str(&u8s!(v.0.0)).unwrap(), i64::from_str(&u8s!(v.0.1)).unwrap(), i64::from_str(&u8s!(v.1.0)).unwrap(), i64::from_str(&u8s!(v.1.1)).unwrap()))
    .map(|v| (v.0 <= v.2 && v.1 >= v.3) || (v.0 >= v.2 && v.1 <= v.3))
    .map(|v| if v { 1 } else { 0 })
    .sum();
  println!("{}", ans);
}