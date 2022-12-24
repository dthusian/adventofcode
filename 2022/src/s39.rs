use bstr::ByteSlice;
use crate::util::input;

fn trollmore(bru: &[(usize, i64)]) -> Vec<i64> {
  bru.iter().map(|v| v.1).collect::<Vec<_>>()
}

fn mix(state: &mut Vec<(usize, i64)>) {
  for i in 0..state.len() {
    let j = state.iter().position(|v| v.0 == i).unwrap();
    let el = state.remove(j);
    let new_idx = (j as i64 + el.1).rem_euclid(state.len() as i64);
    if new_idx == 0 && el.1 < 0 {
      state.push(el);
    } else {
      state.insert(new_idx as usize, el);
    }
  }
}

pub fn main() {
  let mut state = input()
    .split_str("\n")
    .into_iter()
    .map(|v| v.to_str_lossy().parse::<i64>().unwrap() * 811589153)
    .enumerate()
    .collect::<Vec<_>>();
  for _ in 0..10 {
    mix(&mut state);
  }
  let i = state.iter().position(|v| v.1 == 0).unwrap();
  println!("{}", state[(i + 1000) % state.len()].1 + state[(i + 2000) % state.len()].1 + state[(i + 3000) % state.len()].1);
}