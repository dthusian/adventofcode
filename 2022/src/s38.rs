use bstr::ByteSlice;
use crate::util::input;

fn trollmore(bru: &[(usize, i64)]) -> Vec<i64> {
  bru.iter().map(|v| v.1).collect::<Vec<_>>()
}

pub fn main() {
  let mut state = input()
    .split_str("\n")
    .into_iter()
    .map(|v| v.to_str_lossy().parse::<i64>().unwrap())
    .enumerate()
    .collect::<Vec<_>>();
  let state_len = state.len();
  for i in 0..state_len {
    let j = state.iter().position(|v| v.0 == i).unwrap();
    let el = state.remove(j);
    let state_len = state_len as i64;
    let new_idx = j as i64 + el.1;
    let new_idx = (new_idx + state.len() as i64 * 2) % state.len() as i64;
    if new_idx == 0 && el.1 < 0 {
      state.push(el);
    } else {
      state.insert(new_idx as usize, el);
    }
  }
  let i = state.iter().position(|v| v.1 == 0).unwrap();
  println!("{}", state[(i + 1000) % state_len].1 + state[(i + 2000) % state_len].1 + state[(i + 3000) % state_len].1);
}