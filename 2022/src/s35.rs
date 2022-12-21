use std::cmp::max;
use std::collections::{HashMap, HashSet, VecDeque};
use bstr::ByteSlice;
use crate::util::input;

pub fn main() {
  let input = input();
  let cubes = input.split_str("\n")
    .map(|v| v.split_str(",").collect::<Vec<_>>())
    .map(|v| (
      v[0].to_str_lossy().parse::<i64>().unwrap() + 2,
      v[1].to_str_lossy().parse::<i64>().unwrap() + 2,
      v[2].to_str_lossy().parse::<i64>().unwrap() + 2
    ))
    .collect::<HashSet<_>>();
  let drop_size = cubes.iter()
    .fold(0, |acc, v| max(max(v.0, v.1), max(v.2, acc))) + 3;
  let mut exterior_air = HashSet::new();
  let mut visit_queue = VecDeque::new();
  visit_queue.push_back((0i64, 0i64, 0i64));
  while !visit_queue.is_empty() {
    let v = visit_queue.pop_front().unwrap();
    exterior_air.insert(v);
    let v = vec![
      (v.0 + 1, v.1, v.2),
      (v.0 - 1, v.1, v.2),
      (v.0, v.1 + 1, v.2),
      (v.0, v.1 - 1, v.2),
      (v.0, v.1, v.2 + 1),
      (v.0, v.1, v.2 - 1)
    ].into_iter()
      .filter(|v| (0..drop_size).contains(&v.0) && (0..drop_size).contains(&v.1) && (0..drop_size).contains(&v.2))
      .filter(|v| !exterior_air.contains(v))
      .filter(|v| !visit_queue.contains(v))
      .filter(|v| !cubes.contains(v)).collect::<Vec<_>>();
    v.into_iter().for_each(|v| visit_queue.push_back(v));
  }
  let exterior_surface_area = cubes.iter().map(|v| {
    vec![
      (v.0 + 1, v.1, v.2),
      (v.0 - 1, v.1, v.2),
      (v.0, v.1 + 1, v.2),
      (v.0, v.1 - 1, v.2),
      (v.0, v.1, v.2 + 1),
      (v.0, v.1, v.2 - 1)
    ].into_iter().filter(|v| exterior_air.contains(v)).count()
  }).sum::<usize>();
  println!("{}", exterior_surface_area)
}