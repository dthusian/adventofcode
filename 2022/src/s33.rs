use std::cmp::{max, min};
use std::collections::{BTreeMap, HashMap};
use ring::digest::digest;
use crate::util::input;

fn rock_shift_left(rock: &[u8; 4]) -> Option<[u8; 4]> {
  for i in 0..4 {
    if rock[i] & 0x1 != 0 {
      return None
    }
  }
  let mut new_rock = [0; 4];
  for i in 0..4 {
    new_rock[i] = rock[i] >> 1;
  }
  Some(new_rock)
}

fn rock_shift_right(rock: &[u8; 4]) -> Option<[u8; 4]> {
  for i in 0..4 {
    if rock[i] & 0x40 != 0 {
      return None
    }
  }
  let mut new_rock = [0; 4];
  for i in 0..4 {
    new_rock[i] = rock[i] << 1;
  }
  Some(new_rock)
}

// true if no conflict
fn rock_check_conflict(stack: &[u8], offset: usize, rock: &[u8; 4]) -> bool {
  for i in 0..4 {
    if offset + i < stack.len() && (stack[offset + i] & rock[i]) != 0 {
      return false;
    }
  }
  true
}

fn rock_freeze_rock(stack: &mut Vec<u8>, offset: usize, rock: &[u8; 4]) {
  while offset + 4 > stack.len() {
    stack.push(0);
  }
  for i in 0..4 {
    if stack[offset + i] & rock[i] != 0 {
      print_stack(stack, Some(offset), Some(rock));
      panic!("bad");
    }
    stack[offset + i] |= rock[i];
  }
}

fn print_stack(stack: &[u8], offset: Option<usize>, rock: Option<&[u8; 4]>) {
  fn util(b1: u8, b2: u8, mask: u8) -> char {
    let b1 = b1 & mask;
    let b2 = b2 & mask;
    if b1 != 0 && b2 != 0 { '!' } else if b1 != 0 { '#' } else if b2 != 0 { '@' } else { '.' }
  }
  for i in (0..max(Some(stack.len()), offset.map(|v| v + 4)).unwrap()).rev() {
    let b1 = stack.get(i).cloned().or(Some(0)).unwrap();
    let mut b2 = 0;
    if offset.is_some() && i >= offset.unwrap() && i < offset.unwrap() + 4 {
      b2 = rock.unwrap()[i - offset.unwrap()];
    }
    println!("{}{}{}{}{}{}{}",
             util(b1, b2, 0x1),
             util(b1, b2, 0x2),
             util(b1, b2, 0x4),
             util(b1, b2, 0x8),
             util(b1, b2, 0x10),
             util(b1, b2, 0x20),
             util(b1, b2, 0x40),
    )
  }
}

// last 32 rows
fn hash(stack: &[u8]) -> u128 {
  let digest = digest(
    &ring::digest::SHA256,
    &stack[0..min(CHECK_LEN, stack.len())]);
  u128::from_be_bytes((&digest.as_ref()[0..16]).try_into().unwrap()) ^ u128::from_be_bytes((&digest.as_ref()[16..32]).try_into().unwrap())
}

const N: usize = 1_000_000_000_000;
const CHECK_LEN: usize = 200;

pub fn main() {
  let rocks: Vec<[u8; 4]> = vec![
    [15, 0, 0, 0],
    [2, 7, 2, 0],
    [7, 4, 4, 0],
    [1, 1, 1, 1],
    [3, 3, 0, 0]
  ];
  let rock_heights = vec![
    1,
    3,
    3,
    4,
    2
  ];
  let input = input();
  let mut input_idx = 0;
  let mut rock_idx = 0;
  let mut highest_rock = 0; // pretend the floor is at y=0
  let mut secret_height = 0;
  let mut stack = Vec::<u8>::new();
  stack.push(127);
  let mut prev_states = BTreeMap::<(usize, usize, u128), (usize, usize)>::new();
  loop {
    let mut rockpos = highest_rock + 4;
    let mut rock = rocks[rock_idx % 5].clone();
    rock = rock_shift_right(&rock).unwrap();
    rock = rock_shift_right(&rock).unwrap();
    loop {
      // try moving left/right
      let dir = input[input_idx % input.len()];
      input_idx += 1;
      if dir == b'<' {
        if let Some(new_rock) = rock_shift_left(&rock) {
          if rock_check_conflict(&stack, rockpos, &new_rock) {
            rock = new_rock;
          }
        }
      } else if dir == b'>' {
        if let Some(new_rock) = rock_shift_right(&rock) {
          if rock_check_conflict(&stack, rockpos, &new_rock) {
            rock = new_rock;
          }
        }
      } else {
        panic!("invalid input");
      }
      // try moving down
      if rock_check_conflict(&stack, rockpos - 1, &rock) {
        rockpos -= 1;
      } else {
        //print_stack(&stack, Some(rockpos), Some(&rock));
        rock_freeze_rock(&mut stack, rockpos, &rock);
        highest_rock = max(rockpos + rock_heights[rock_idx % 5] - 1, highest_rock);
        break;
      }
    }
    rock_idx += 1;
    if rock_idx == N {
      break;
    }
    if rock_idx > N {
      panic!("bad");
    }
    if secret_height == 0 {
      let key_state = (input_idx % input.len(), rock_idx % 5, hash(&stack[highest_rock.checked_sub(CHECK_LEN).unwrap_or(0)..]));
      let val_state = (rock_idx, highest_rock);
      if let Some(found_state) = prev_states.get(&key_state) {
        let d_rock_idx = rock_idx - found_state.0;
        if d_rock_idx > 100 {
          let d_height = highest_rock - found_state.1;
          println!("cycle found! len={} start={} d_height={}", d_rock_idx, found_state.0, d_height);
          let max_cycles_executable = (N - rock_idx) / d_rock_idx - 5;
          println!("can execute {} times", max_cycles_executable);
          secret_height = max_cycles_executable * d_height;
          rock_idx += max_cycles_executable * d_rock_idx;
        }
      } else {
        prev_states.insert(key_state, val_state);
      }
    }
  }
  //print_stack(&stack, None, None);
  println!("{}", highest_rock + secret_height);
}