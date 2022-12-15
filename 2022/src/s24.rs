use std::cmp::{min, Ordering};
use bstr::ByteSlice;
use serde_json::{Value};
use crate::util::input;

// least-to-greatest
fn cmp(a: &Value, b: &Value) -> Ordering {
  if a.is_i64() && b.is_i64() {
    a.as_i64().unwrap().cmp(&b.as_i64().unwrap())
  } else if a.is_array() && b.is_array() {
    let a = a.as_array().unwrap();
    let b = b.as_array().unwrap();
    for i in 0..min(a.len(), b.len()) {
      let cmp = cmp(&a[i], &b[i]);
      if cmp != Ordering::Equal {
        return cmp;
      }
    }
    a.len().cmp(&b.len())
  } else if a.is_i64() && b.is_array() {
    cmp(&serde_json::to_value(vec![a.as_i64().unwrap()]).unwrap(), &b)
  } else if a.is_array() && b.is_i64() {
    cmp(&a, &serde_json::to_value(vec![b.as_i64().unwrap()]).unwrap())
  } else {
    panic!("bruh");
  }
}

pub fn main() {
  let input = input();
  let num_sorted = input.split_str("\n\n")
    .map(|v| v.split_str("\n").collect::<Vec<_>>())
    .map(|v| (v[0], v[1]))
    .map(|v| (serde_json::from_slice::<Value>(v.0).unwrap(), serde_json::from_slice::<Value>(v.1).unwrap()))
    .map(|v| {
      cmp(&v.0, &v.1) != Ordering::Greater
    })
    .enumerate()
    .map(|v| { if v.1 { v.0 + 1 } else { 0 } })
    .sum::<usize>();
  println!("{}", num_sorted);
}