use std::collections::{HashMap, HashSet};
use bstr::ByteSlice;
use crate::util::input;

fn propose(round: i64, elves: &HashSet<(i64, i64)>, pos: (i64, i64)) -> (i64, i64) {
  let dirs = b"NSWE";
  let mut has_neighbor = false;
  for i in -1..=1 {
    for j in -1..=1 {
      if i != 0 || j != 0 {
        if elves.contains(&(pos.0 + i, pos.1 + j)) {
          has_neighbor = true;
          break;
        }
      }
    }
  }
  if !has_neighbor {
    return pos;
  }
  for i in 0..4 {
    let dir = dirs[((round + i) % 4) as usize];
    if dir == b'N' {
      if !elves.contains(&(pos.0 - 1, pos.1 - 1)) &&
        !elves.contains(&(pos.0, pos.1 - 1)) &&
        !elves.contains(&(pos.0 + 1, pos.1 - 1)) {
        return (pos.0, pos.1 - 1);
      }
    } else if dir == b'S' {
      if !elves.contains(&(pos.0 - 1, pos.1 + 1)) &&
        !elves.contains(&(pos.0, pos.1 + 1)) &&
        !elves.contains(&(pos.0 + 1, pos.1 + 1)) {
        return (pos.0, pos.1 + 1);
      }
    } else if dir == b'W' {
      if !elves.contains(&(pos.0 - 1, pos.1 - 1)) &&
        !elves.contains(&(pos.0 - 1, pos.1)) &&
        !elves.contains(&(pos.0 - 1, pos.1 + 1)) {
        return (pos.0 - 1, pos.1);
      }
    } else if dir == b'E' {
      if !elves.contains(&(pos.0 + 1, pos.1 - 1)) &&
        !elves.contains(&(pos.0 + 1, pos.1)) &&
        !elves.contains(&(pos.0 + 1, pos.1 + 1)) {
        return (pos.0 + 1, pos.1);
      }
    } else {
      panic!("bad");
    }
  }
  pos
}

fn get_bounds(elves: &HashSet<(i64, i64)>) -> (i64, i64, i64, i64) {
  let min_x = elves.iter().map(|v| v.0).min().unwrap();
  let max_x = elves.iter().map(|v| v.0).max().unwrap();
  let min_y = elves.iter().map(|v| v.1).min().unwrap();
  let max_y = elves.iter().map(|v| v.1).max().unwrap();
  (min_x, max_x, min_y, max_y)
}

fn print_elves(elves: &HashSet<(i64, i64)>) {
  let bounds = get_bounds(elves);
  for y in bounds.2..=bounds.3 {
    for x in bounds.0..=bounds.1 {
      print!("{}", if elves.contains(&(x, y)) { '#' } else { '.' });
    }
    println!();
  }
}

pub fn main() {
  let mut elves = HashSet::new();
  input()
    .split_str("\n")
    .enumerate()
    .for_each(|v| {
      v.1.into_iter()
        .enumerate()
        .for_each(|v2| {
          if *v2.1 == b'#' {
            elves.insert((v2.0 as i64, v.0 as i64));
          }
        });
    });
  let arknights = elves.len();
  let mut round = 0;
  loop {
    // new_pos -> original_pos
    let mut move_set = HashMap::new();
    let mut conflict_set = HashSet::new();
    let mut elf_moved = false;
    for elf in &elves {
      let proposal = propose(round, &elves, *elf);
      if proposal != *elf {
        elf_moved = true;
      }
      if !conflict_set.contains(&proposal) {
        if move_set.contains_key(&proposal) {
          move_set.remove(&proposal);
          conflict_set.insert(proposal);
        } else {
          move_set.insert(proposal, *elf);
        }
      }
    }
    if !elf_moved { break; }
    move_set.values().for_each(|v| { elves.remove(v); });
    move_set.into_keys().for_each(|v| { elves.insert(v); });
    if elves.len() != arknights {
      panic!("elf lost :interrobang:")
    }
    round += 1;
  }
  println!("{}", round + 1);
}