// A* my beloved

use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use bstr::{BString, ByteSlice};
use num_complex::ComplexFloat;
use crate::util::input;

fn print_map(map: &Vec<Vec<u8>>) {
  map.iter().for_each(|v| {
    println!("{}", v.to_str_lossy());
  });
}

struct MapState {
  // (vx, vy, xoff, yoff)
  winds: Vec<(i64, i64, i64, i64)>,
  map_cache: HashMap<i64, Vec<Vec<u8>>>,
  map_size: (i64, i64)
}

impl MapState {
  pub fn new(map: Vec<BString>) -> Self {
    MapState {
      winds: map.iter()
        .skip(1)
        .enumerate()
        .map(|v| {
          v.1.iter()
            .skip(1)
            .enumerate()
            .map(|v2| {
              if *v2.1 == b'.' || *v2.1 == b'#' {
                return None
              }
              let dir = match *v2.1 {
                b'^' => (0, -1),
                b'>' => (1, 0),
                b'v' => (0, 1),
                b'<' => (-1, 0),
                _ => panic!("bruh")
              };
              Some((dir.0, dir.1, v2.0 as i64, v.0 as i64))
            }).collect::<Vec<_>>()
        })
        .flatten()
        .filter_map(|v| v)
        .collect(),
      map_cache: Default::default(),
      map_size: (map[0].len() as i64 - 2, map.len() as i64 - 2)
    }
  }

  pub fn generate(&mut self, time: i64) -> &Vec<Vec<u8>> {
    if self.map_cache.contains_key(&time) {
      return &self.map_cache[&time];
    }
    let mut new_map = vec![vec![b'.'; self.map_size.0 as usize + 2]; self.map_size.1 as usize + 2];
    // draw walls
    for i in 0..self.map_size.0 + 2 {
      new_map[0][i as usize] = b'#';
      new_map[self.map_size.1 as usize + 1][i as usize] = b'#';
    }
    for i in 0..self.map_size.1 + 2 {
      new_map[i as usize][0] = b'#';
      new_map[i as usize][self.map_size.0 as usize + 1] = b'#';
    }
    new_map[0][1] = b'.';
    new_map[self.map_size.1 as usize + 1][self.map_size.0 as usize] = b'.';
    // wind
    for wind in &self.winds {
      let (vx, vy, xoff, yoff) = *wind;
      let x = (vx * time + xoff).rem_euclid(self.map_size.0);
      let y = (vy * time + yoff).rem_euclid(self.map_size.1);
      new_map[y as usize + 1][x as usize + 1] = b'*';
    }
    self.map_cache.insert(time, new_map);
    &self.map_cache[&time]
  }
}

// x, y, t
type Node = (i64, i64, i64);

#[derive(Copy, Clone, Debug, PartialEq)]
struct ScoredNode(Node, f32);

impl Eq for ScoredNode { }

impl PartialOrd<Self> for ScoredNode {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for ScoredNode {
  fn cmp(&self, other: &Self) -> Ordering {
    let a = if self.1.is_nan() { f32::NEG_INFINITY } else { self.1 };
    let b = if other.1.is_nan() { f32::NEG_INFINITY } else { other.1 };
    b.partial_cmp(&a).unwrap()
  }
}

pub fn euclidean_dist_heuristic(node: Node, exit: (i64, i64)) -> f32 {
  let delta_x = node.0 - exit.0;
  let delta_y = node.1 - exit.1;
  ((delta_x * delta_x + delta_y * delta_y) as f32).sqrt()
}

fn a_star(map: &mut MapState, debug: bool) -> i64 {
  let start = (1, 0, 0);
  let exit_coords = (map.map_size.0, map.map_size.1 + 1);
  let mut pq = BinaryHeap::new();
  pq.push(ScoredNode(start, 0.0));
  let mut g_score = HashMap::new();
  g_score.insert(start, 0);
  let mut prev = HashMap::new();
  while !pq.is_empty() {
    let scored_node = pq.pop().unwrap();
    let node = scored_node.0;
    if (node.0, node.1) == exit_coords {
      if debug {
        let mut track_node = node;
        let mut path = Vec::new();
        while track_node != start {
          path.push(track_node);
          track_node = prev[&track_node];
        }
        path.push(track_node);
        path.reverse();
        path.into_iter().for_each(|v| println!("{:?}", v));
      }
      return g_score[&node];
    }
    [
      (node.0 - 1, node.1 * 1, node.2 + 1),
      (node.0 * 1, node.1 - 1, node.2 + 1),
      (node.0 * 1, node.1 * 1, node.2 + 1),
      (node.0 * 1, node.1 + 1, node.2 + 1),
      (node.0 + 1, node.1 * 1, node.2 + 1)
    ].into_iter().filter(|v| {
      v.0 >= 0 && v.1 >= 0 && v.0 < map.map_size.0 + 2 && v.1 < map.map_size.1 + 2 && map.generate(v.2)[v.1 as usize][v.0 as usize] == b'.'
    }).for_each(|v| {
      let maybe_g_score = g_score[&node] + 1;
      if !g_score.contains_key(&v) || maybe_g_score < g_score[&v] {
        if debug {
          prev.insert(v, node);
        }
        g_score.insert(v, maybe_g_score);
        let h_score = euclidean_dist_heuristic(v, exit_coords);
        pq.push(ScoredNode(v, maybe_g_score as f32 + h_score));
      }
    });
  }
  panic!("destination unreachable")
}

pub fn main() {
  let input = input();
  let mut maps = MapState::new(input.split_str("\n").map(|v| v.into()).collect::<Vec<_>>());
  println!("{}", a_star(&mut maps, false));
}