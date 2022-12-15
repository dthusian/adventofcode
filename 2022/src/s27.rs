use std::cmp::{max, min};
use bstr::ByteSlice;
use crate::util::input;

fn fillv(bitmap: &mut Vec<Vec<u8>>, col: usize, from: usize, to: usize) {
  let min = min(from, to);
  let max = max(from, to);
  for i in min..=max {
    bitmap[col][i] = b'#';
  }
}

fn fillh(bitmap: &mut Vec<Vec<u8>>, row: usize, from: usize, to: usize) {
  let min = min(from, to);
  let max = max(from, to);
  for i in min..=max {
    bitmap[i][row] = b'#';
  }
}

fn print_bitmap(bitmap: &Vec<Vec<u8>>) {
  bitmap.iter().for_each(|v| println!("{}", v.to_str_lossy()));
}

pub fn main() {
  let input = input();
  let mut bitmap = vec![vec![b'.'; 1000]; 200];
  let mut max_y = 0;
  input.split_str("\n").for_each(|v| {
    let movements = v.split_str(" -> ")
      .map(|v| v.split_once_str(",").unwrap())
      .map(|v| (v.0.to_str_lossy().parse::<usize>().unwrap(), v.1.to_str_lossy().parse::<usize>().unwrap()))
      .collect::<Vec<_>>();
    bitmap[movements[0].1][movements[0].0] = b'#';
    for i in 1..movements.len() {
      let from = movements[i - 1];
      let to = movements[i];
      if to.1 > max_y { max_y = to.1 }
      if from.0 == to.0 {
        fillh(&mut bitmap, from.0, from.1, to.1);
      } else if from.1 == to.1 {
        fillv(&mut bitmap, from.1, from.0, to.0);
      } else {
        panic!("diagonal line");
      }
    }
  });
  fillv(&mut bitmap, max_y + 2, 0, 999);
  let mut sand_produced = 0;
  loop {
    let mut pos = (500, 0);
    if bitmap[pos.1][pos.0] == b'o' {
      // full
      break;
    }
    loop {
      // fall downwards
      if bitmap[pos.1 + 1][pos.0] == b'.' {
        pos.1 += 1;
      } else if bitmap[pos.1 + 1][pos.0 - 1] == b'.' {
        pos.1 += 1;
        pos.0 -= 1;
      } else if bitmap[pos.1 + 1][pos.0 + 1] == b'.' {
        pos.1 += 1;
        pos.0 += 1;
      } else {
        // get stuck
        bitmap[pos.1][pos.0] = b'o';
        break;
      }
    }
    sand_produced += 1;
  }
  println!("{}", sand_produced);
}