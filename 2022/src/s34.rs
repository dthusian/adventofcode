use std::collections::HashSet;
use bstr::ByteSlice;
use crate::util::input;

pub fn main() {
  let input = input();
  let cubes = input.split_str("\n")
    .map(|v| v.split_str(",").collect::<Vec<_>>())
    .map(|v| (
      v[0].to_str_lossy().parse::<i64>().unwrap(),
      v[1].to_str_lossy().parse::<i64>().unwrap(),
      v[2].to_str_lossy().parse::<i64>().unwrap()
    ))
    .collect::<HashSet<_>>();
  let surface_area = cubes.iter().map(|v| {
    vec![
      cubes.contains(&(v.0 + 1, v.1, v.2)),
      cubes.contains(&(v.0 - 1, v.1, v.2)),
      cubes.contains(&(v.0, v.1 + 1, v.2)),
      cubes.contains(&(v.0, v.1 - 1, v.2)),
      cubes.contains(&(v.0, v.1, v.2 + 1)),
      cubes.contains(&(v.0, v.1, v.2 - 1))
    ].iter().filter(|v| !**v).count()
  }).sum::<usize>();
  println!("{}", surface_area);
}