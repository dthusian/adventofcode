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
  let mut knots = vec![Vec2::default(); 10]; // 0 is head
  input.split_str("\n").for_each(|v| {
    let spl = v.split_str(" ").collect::<Vec<_>>();
    let mov = Vec2::from_chr(spl[0][0]) * spl[1].to_str_lossy().parse().unwrap();
    let target = knots[0] + mov;
    while knots[0] != target {
      knots[0].follow(target);
      for i in 1..knots.len() {
        let next = knots[i - 1].clone();
        while !knots[i].close_enough(next) {
          knots[i].follow(next);
          if i == 9 {
            visited.insert(knots[i]);
          }
        }
      }
    }
  });
  let mut grid = vec![vec!['.'; 30]; 30];
  visited.iter().for_each(|v| {
    let pos = *v + Vec2(15, 15);
    if pos.0 >= 0 && pos.0 < 30 && pos.1 >= 0 && pos.1 < 30 {
      grid[pos.1 as usize][pos.0 as usize] = '#';
    }
  });
  println!("{}", grid.into_iter().map(|v| v.iter().collect::<String>()).reduce(|a, b| a + "\n" + &b).unwrap());
  println!("{}", visited.len());
}