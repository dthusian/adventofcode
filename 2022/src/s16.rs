use std::cmp::max;
use std::collections::BTreeSet;
use std::ops::{Add, AddAssign, Mul};
use bstr::ByteSlice;
use crate::util::input;

#[derive(Copy, Clone, Debug, Default, Ord, PartialOrd, Eq, PartialEq)]
struct Vec2(pub i64, pub i64);

impl Vec2 {
  pub fn from_chr(chr: u8) -> Vec2 {
    match chr {
      b'U' => Vec2(0, -1),
      b'D' => Vec2(0, 1),
      b'L' => Vec2(-1, 0),
      b'R' => Vec2(1, 0),
      _ => panic!("invalid direction")
    }
  }

  pub fn follow(&mut self, leader: Vec2) {
    let motion = Vec2((leader.0 - self.0).signum(), (leader.1 - self.1).signum());
    *self += motion;
  }

  pub fn close_enough(&mut self, other: Vec2) -> bool {
    max((self.0 - other.0).abs(), (self.1 - other.1).abs()) <= 1
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

pub fn main() {
  let input = input();
  let mut visited = BTreeSet::<Vec2>::new();
  visited.insert(Vec2(0, 0));
  let mut tail = Vec2(0, 0);
  let mut head = Vec2(0, 0);
  input.split_str("\n").for_each(|v| {
    let spl = v.split_str(" ").collect::<Vec<_>>();
    let mov = Vec2::from_chr(spl[0][0]) * spl[1].to_str_lossy().parse().unwrap();
    head += mov;
    while !tail.close_enough(head) {
      tail.follow(head);
      visited.insert(tail);
    }
  });
  println!("{}", visited.len());
}