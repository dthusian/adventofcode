use std::cmp::{max, min};
use std::collections::HashSet;
use bstr::{BString, ByteSlice};
use crate::util::input;

const SEARCH_RANGE: i64 = 4_000_000;

#[derive(Debug, Default, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Range(pub i64, pub i64);

impl Range {
  pub fn new(a: i64, b: i64) -> Range {
    Range(min(a, b), max(a, b))
  }

  fn check_validity(&self) {
    if self.0 > self.1 {
      panic!("Invalidly constructed range")
    }
  }

  pub fn union(&self, other: &Range) -> Option<Range> {
    self.check_validity();
    other.check_validity();
    if self.1 + 1 < other.0 || self.0 > other.1 + 1 {
      None
    } else {
      Some(Range::new(min(self.0, other.0), max(self.1, other.1)))
    }
  }

  pub fn len(&self) -> i64 {
    self.1 - self.0 + 1
  }
}

#[derive(Debug, Default, Clone)]
struct RangeSet(pub Vec<Range>);

impl RangeSet {
  pub fn add(&mut self, range: Range) {
    let mut included = false;
    for i in 0..self.0.len() {
      let merged_range = self.0[i].union(&range);
      if let Some(merged_range) = merged_range {
        included = true;
        self.0.remove(i);
        self.add(merged_range);
        break;
      }
    }
    if !included {
      self.0.push(range);
    }
  }

  pub fn total_len(&self) -> i64 {
    self.0.iter().map(|v| v.len()).sum()
  }

  pub fn diff(&mut self, range: Range) -> RangeSet {
    let mut rangeset = RangeSet::default();
    self.0.sort();
    let mut i = range.0;
    for range2 in &self.0 {
      if range2.0 < i && range2.1 < i {
        continue;
      }
      if range2.0 <= i && range2.1 >= i {
        i = range2.1 + 1;
        continue;
      }
      rangeset.0.push(Range::new(i, range2.0 - 1));
      i = range2.1 + 1;
    }
    if i <= range.1 {
      rangeset.0.push(Range::new(i, range.1))
    }
    rangeset
  }
}

fn sensor_to_range(sensor: (i64, i64, i64), row: i64) -> Option<Range> {
  let y_offset = (sensor.1 - row).abs();
  if y_offset > sensor.2 {
    None
  } else {
    Some(Range::new(sensor.0 - (sensor.2 - y_offset), sensor.0 + (sensor.2 - y_offset)))
  }
}

fn consider_row(sensors: &[(i64, i64, i64)], _known_beacons: &HashSet<(i64, i64)>, row: i64) -> RangeSet {
  let mut rangeset = RangeSet::default();
  sensors.iter()
    .filter_map(|v| sensor_to_range(v.clone(), row))
    .for_each(|v| rangeset.add(v));
  rangeset.diff(Range::new(0, SEARCH_RANGE))
}

fn parse(input: BString) -> (Vec<(i64, i64, i64)>, HashSet<(i64, i64)>) {
  let raw_tuples = input.split_str("\n")
    .map(|v| v.split_str(" ").collect::<Vec<_>>())
    .map(|v| (
      v[2].strip_prefix(b"x=").unwrap().strip_suffix(b",").unwrap(),
      v[3].strip_prefix(b"y=").unwrap().strip_suffix(b":").unwrap(),
      v[8].strip_prefix(b"x=").unwrap().strip_suffix(b",").unwrap(),
      v[9].strip_prefix(b"y=").unwrap()
    ))
    .map(|v| (
      v.0.to_str_lossy().parse::<i64>().unwrap(),
      v.1.to_str_lossy().parse::<i64>().unwrap(),
      v.2.to_str_lossy().parse::<i64>().unwrap(),
      v.3.to_str_lossy().parse::<i64>().unwrap()
    ))
    .collect::<Vec<_>>();
  let sensors = raw_tuples.iter()
    .map(|v| (v.0, v.1, (v.0 - v.2).abs() + (v.1 - v.3).abs()))
    .collect::<Vec<_>>();
  let known_beacons = raw_tuples.iter()
    .map(|v| (v.2, v.3))
    .collect::<HashSet<_>>();
  (sensors, known_beacons)
}

pub fn main() {
  let (sensors, known_beacons) = parse(input());
  for i in 0..SEARCH_RANGE {
    let maybe_beacon = consider_row(&sensors, &known_beacons, i);
    if maybe_beacon.0.len() != 0 {
      println!("{}", maybe_beacon.0[0].0 * 4_000_000 + i);
      break;
    }
  }
}