use std::collections::HashMap;
use std::ops::{Add, AddAssign, Div, Mul, Neg, Rem};
use bstr::{BString, ByteSlice};
use crate::util::input;

#[derive(Copy, Clone, Debug, Default, Ord, PartialOrd, Eq, PartialEq, Hash)]
struct Vec2(pub i64, pub i64);

impl Vec2 {
  pub fn rotate(self, deg: i64) -> Vec2 {
    match deg.rem_euclid(360) {
      0 => self,
      90 => Vec2(-self.1, self.0),
      180 => Vec2(-self.0, -self.1),
      270 => Vec2(self.1, -self.0),
      _ => panic!("bruh")
    }
  }

  pub fn deg_to_vec(deg: i64) -> Vec2 {
    match deg.rem_euclid(360) {
      0 => Vec2(0, -1),
      90 => Vec2(1, 0),
      180 => Vec2(0, 1),
      270 => Vec2(-1, 0),
      _ => panic!("bruh")
    }
  }

  pub fn deg_to_dir_score(deg: i64) -> i64 {
    match deg.rem_euclid(360) {
      90 => 0,
      180 => 1,
      270 => 2,
      0 => 3,
      _ => panic!("bruh")
    }
  }

  pub fn modulo(self, x: Vec2) -> Vec2 {
    Vec2(self.0.rem_euclid(x.0), self.1.rem_euclid(x.1))
  }

  pub fn dir_rot(self, dir: i64, target_dir: i64) -> Vec2 {
    let diff = target_dir - dir;
    self.rotate(diff)
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

impl Div<i64> for Vec2 {
  type Output = Vec2;

  fn div(self, rhs: i64) -> Self::Output {
    Vec2(((self.0 as f64) / (rhs as f64)).floor() as i64, ((self.1 as f64) / (rhs as f64)).floor() as i64)
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

fn stitch_side(mapping: &mut HashMap<(i64, Vec2), (i64, Vec2)>, side_a: (i64, Vec2, i64), side_b: (i64, Vec2, i64), len: i64) {
  for i in 0..len {
    let pos_a = side_a.1 + Vec2::deg_to_vec(side_a.2) * i;
    let pos_b = side_b.1 + Vec2::deg_to_vec(side_b.2) * i;
    mapping.insert((side_a.0, pos_a), ((side_b.0 + 180).rem_euclid(360), pos_b));
    mapping.insert((side_b.0, pos_b), ((side_a.0 + 180).rem_euclid(360), pos_a));
  }
}

pub fn main() {
  // hardcoding :trol:
  // map (dir, tile_coord) -> (new dir, tile_coord)
  let mut m_test = HashMap::<(i64, Vec2), (i64, Vec2)>::new();
  stitch_side(&mut m_test, (0, Vec2(7, 3), 270), (270, Vec2(7, 3), 0), 4);
  stitch_side(&mut m_test, (90, Vec2(12, 7), 0), (0, Vec2(12, 7), 90), 4);
  stitch_side(&mut m_test, (180, Vec2(7, 8), 270), (270, Vec2(7, 8), 180), 4);

  stitch_side(&mut m_test, (0, Vec2(8, -1), 90), (0, Vec2(3, 3), 270), 4);
  stitch_side(&mut m_test, (90, Vec2(12, 0), 180), (90, Vec2(16, 11), 0), 4);
  stitch_side(&mut m_test, (180, Vec2(3, 8), 270), (180, Vec2(8, 12), 90), 4);

  stitch_side(&mut m_test, (270, Vec2(-1, 7), 0), (180, Vec2(12, 12), 90), 4);
  let mut m_real = HashMap::<(i64, Vec2), (i64, Vec2)>::new();
  stitch_side(&mut m_real, (0, Vec2(49, 99), 270), (270, Vec2(49, 99), 0), 50);
  stitch_side(&mut m_real, (90, Vec2(100, 50), 180), (180, Vec2(100, 50), 90), 50);
  stitch_side(&mut m_real, (90, Vec2(50, 150), 180), (180, Vec2(50, 150), 90), 50);

  stitch_side(&mut m_real, (270, Vec2(49, 49), 0), (270, Vec2(-1, 100), 180), 50);
  stitch_side(&mut m_real, (90, Vec2(150, 49), 0), (90, Vec2(100, 100), 180), 50);

  stitch_side(&mut m_real, (0, Vec2(50, -1), 90), (270, Vec2(-1, 150), 180), 50);
  stitch_side(&mut m_real, (0, Vec2(150, -1), 270), (180, Vec2(49, 200), 270), 50);
  let mapping = m_real;
  // end hardcoding

  let (mut map, instr) = parse(input());
  let width = map.iter().map(|v| v.len()).max().unwrap();
  map.iter_mut().for_each(|v| {
    while v.len() < width {
      v.push(b' ');
    }
  });
  let mut rip = 0;
  let mut dir = 90;
  let mut pos = Vec2(map[0].iter().position(|v| *v != b' ').unwrap() as i64, 0);
  print_board(&map, pos);
  println!("\n");
  while rip < instr.len() {
    if instr[rip].is_ascii_digit() {
      // parse
      let len = instr[rip..].iter().position(|v| v.is_ascii_alphabetic()).or(Some(instr.len() - rip)).unwrap();
      let x = instr[rip..rip + len].to_str_lossy().parse::<usize>().unwrap();
      rip += len;
      // exec
      for _ in 0..x {
        let prev_pos = pos;
        let prev_dir = dir;
        pos += Vec2::deg_to_vec(dir);
        let maybe_mapping = mapping.get(&(dir, pos));
        if let Some(mapping) = maybe_mapping {
          //println!("tp {} {:?} -> {} {:?}", dir, pos, mapping.0, mapping.1);
          dir = mapping.0;
          pos = mapping.1;
          pos += Vec2::deg_to_vec(dir);
        }
        if map[pos.1 as usize][pos.0 as usize] == b' ' {
          panic!("bad");
        }
        if map[pos.1 as usize][pos.0 as usize] == b'#' {
          pos = prev_pos;
          dir = prev_dir;
          break;
        }
      }
      print_board(&map, pos);
      println!("\n");
    } else if instr[rip].is_ascii_alphabetic() {
      // parse
      let rot = instr[rip];
      rip += 1;
      // exec
      if rot == b'L' {
        dir = (dir - 90).rem_euclid(360);
      } else if rot == b'R' {
        dir = (dir + 90).rem_euclid(360);
      } else {
        panic!("breh");
      }
    } else {
      panic!("bad");
    }
  }
  let dir_score = Vec2::deg_to_dir_score(dir);
  let password = dir_score + (pos.0 + 1) * 4 + (pos.1 + 1) * 1000;
  println!("{}", password);
}