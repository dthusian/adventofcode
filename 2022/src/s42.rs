use std::ops::{Add, AddAssign, Mul, Neg};
use bstr::{BString, ByteSlice};
use crate::util::input;

#[derive(Copy, Clone, Debug, Default, Ord, PartialOrd, Eq, PartialEq)]
struct Vec2(pub i64, pub i64);

impl Vec2 {
  pub fn rotate_left(self) -> Vec2 {
    Vec2(self.1, -self.0)
  }

  pub fn rotate_right(self) -> Vec2 {
    Vec2(-self.1, self.0)
  }

  pub fn modulo(self, x: Vec2) -> Vec2 {
    Vec2(self.0.rem_euclid(x.0), self.1.rem_euclid(x.1))
  }

  pub fn dir_char(self) -> u8 {
    match self {
      Vec2(1, 0) => b'>',
      Vec2(0, 1) => b'v',
      Vec2(-1, 0) => b'<',
      Vec2(0, -1) => b'^',
      Vec2(_, _) => b'!'
    }
  }
}

impl Add for Vec2 {
  type Output = Vec2;

  fn add(self, rhs: Self) -> Self::Output {
    Vec2(self.0 + rhs.0, self.1 + rhs.1)
  }
}

impl Mul<i64> for Vec2 {
  type Output = Vec2;

  fn mul(self, rhs: i64) -> Self::Output {
    Vec2(self.0 * rhs, self.1 * rhs)
  }
}

impl AddAssign for Vec2 {
  fn add_assign(&mut self, rhs: Self) {
    self.0 += rhs.0;
    self.1 += rhs.1;
  }
}

impl Neg for Vec2 {
  type Output = Vec2;

  fn neg(self) -> Self::Output {
    Vec2(-self.0, -self.1)
  }
}

fn parse(input: BString) -> (Vec<BString>, BString) {
  let parts = input.split_once_str("\n\n").unwrap();
  (parts.0.split_str("\n").map(|v| v.into()).collect::<Vec<_>>(), parts.1.into())
}

fn print_board(map: &Vec<BString>, pos: Vec2) {
  map.clone()
    .into_iter()
    .enumerate()
    .for_each(|mut v| {
      if v.0 as i64 == pos.1 {
        v.1[pos.0 as usize] = b'@';
      }
      println!("{}", v.1.to_str_lossy());
    });
}

pub fn main() {
  let (mut map, instr) = parse(input());
  let width = map.iter().map(|v| v.len()).max().unwrap();
  map.iter_mut().for_each(|v| {
    while v.len() < width {
      v.push(b' ');
    }
  });
  let map_size = Vec2(width as i64, map.len() as i64);
  let mut rip = 0;
  let mut dir = Vec2(1, 0);
  let mut pos = Vec2(map[0].iter().position(|v| *v != b' ').unwrap() as i64, 0);
  //print_board(&map, pos);
  //println!("\n");
  while rip < instr.len() {
    if instr[rip].is_ascii_digit() {
      let len = instr[rip..].iter().position(|v| v.is_ascii_alphabetic()).or(Some(instr.len() - rip)).unwrap();
      let x = instr[rip..rip + len].to_str_lossy().parse::<usize>().unwrap();
      rip += len;
      for _ in 0..x {
        let prev_pos = pos;
        pos += dir;
        pos = pos.modulo(map_size);
        while map[pos.1 as usize][pos.0 as usize] == b' ' {
          pos += dir;
          pos = pos.modulo(map_size);
        }
        if map[pos.1 as usize][pos.0 as usize] == b'#' {
          pos = prev_pos;
          break;
        }
      }
      //print_board(&map, pos);
      //println!("{}\n\n", dir.dir_char() as char);
    } else if instr[rip].is_ascii_alphabetic() {
      let rot = instr[rip];
      if rot == b'L' {
        dir = dir.rotate_left();
      } else if rot == b'R' {
        dir = dir.rotate_right();
      } else {
        panic!("breh");
      }
      rip += 1;
    } else {
      panic!("bad");
    }
  }
  let dir_score = match dir {
    Vec2(1, 0) => 0,
    Vec2(0, 1) => 1,
    Vec2(-1, 0) => 2,
    Vec2(0, -1) => 3,
    Vec2(_, _) => panic!("bruh")
  };
  let password = dir_score + (pos.0 + 1) * 4 + (pos.1 + 1) * 1000;
  println!("{}", password);
}