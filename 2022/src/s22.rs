use std::collections::{BTreeSet, HashMap, VecDeque};
use bstr::ByteSlice;
use crate::util::input;

fn find(input: &[Vec<u8>], chr: u8) -> (usize, usize) {
  let y = input.iter().position(|v| v.contains(&chr)).unwrap();
  let x = input[y].iter().position(|v| *v == chr).unwrap();
  (x, y)
}

pub fn main() {
  let raw_input = input();
  let mut input = raw_input.split_str("\n").map(|v| v.to_vec()).collect::<Vec<_>>();
  let start = find(&input, b'S');
  let end = find(&input, b'E');
  input[start.1][start.0] = b'a';
  input[end.1][end.0] = b'z';
  let mut adj = HashMap::new();
  for y in 0..input.len() {
    for x in 0..input[0].len() {
      let h = input[y][x];
      let mut v = vec![];
      if x != 0 && (h + 1 >= input[y][x - 1]) {
        v.push((x - 1, y));
      }
      if x != input[0].len() - 1 && (h + 1 >= input[y][x + 1]) {
        v.push((x + 1, y));
      }
      if y != 0 && (h + 1 >= input[y - 1][x]) {
        v.push((x, y - 1));
      }
      if y != input.len() - 1 && (h + 1 >= input[y + 1][x]) {
        v.push((x, y + 1));
      }
      adj.insert((x, y), v);
    }
  }
  let mut visited = BTreeSet::new();
  let mut to_visit = VecDeque::new();
  let mut steps = HashMap::new();
  to_visit.push_back((start, 0));
  visited.insert(start);
  while !to_visit.is_empty() {
    let (visit, depth) = to_visit.pop_front().unwrap();
    if visit == end {
      println!("{}", depth);
      break;
    }
    for neighbor in adj.get(&visit).unwrap() {
      if !visited.contains(neighbor) {
        visited.insert(*neighbor);
        steps.insert(neighbor, visit);
        to_visit.push_back((*neighbor, depth + 1));
      }
    }
  }
}