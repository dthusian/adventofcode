use std::cmp::max;
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
  let mut highest_rock = 0; // pretend the floor is at y=0
  let mut stack = Vec::<u8>::new();
  stack.push(127);
  for i in 0..2022 {
    let mut rockpos = highest_rock + 4;
    let mut rock = rocks[i % 5].clone();
    rock = rock_shift_right(&rock).unwrap();
    rock = rock_shift_right(&rock).unwrap();
    loop {
      // try moving left/right
      let dir = input[input_idx];
      input_idx += 1;
      input_idx %= input.len();
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
        highest_rock = max(rockpos + rock_heights[i % 5] - 1, highest_rock);
        break;
      }
      //print_stack(&stack, Some(rockpos), Some(&rock));
    }
    //print_stack(&stack, None, None);
    //println!("{}", input_idx);
  }
  //print_stack(&stack, None, None);
  println!("expected popcount: {}", 22 * (2022 / 5) + 9 + 7);
  println!("real popcount: {}", stack.iter().map(|v| (v & 0x7f).count_ones()).sum::<u32>());
  println!("{}", highest_rock);
}