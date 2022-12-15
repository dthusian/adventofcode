use std::cmp::{min, Ordering};
use bstr::{BString, ByteSlice};
use itermore::IterSorted;
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
  let mut input = input();
  input.append(&mut *BString::from("\n[[2]]\n[[6]]"));
  let packets = input.split_str("\n").filter(|v| v.len() > 0)
    .map(|v| serde_json::from_slice::<Value>(v).unwrap())
    .sorted_by(|a, b| cmp(a, b))
    .collect::<Vec<_>>();
  let start = packets.iter().position(|v| v.to_string() == "[[2]]").unwrap();
  let end = packets.iter().position(|v| v.to_string() == "[[6]]").unwrap();
  println!("{}", (start + 1) * (end + 1));
}