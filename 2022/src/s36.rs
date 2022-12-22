use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, VecDeque};
use bstr::ByteSlice;
use rayon::iter::ParallelBridge;
use rayon::iter::ParallelIterator;
use crate::util::input;

const OUTPUT_PATHS: bool = true;

const MAX_TIME: i64 = 24;

const B_ORE_BOT_ORE: usize = 0;
const B_CLAY_BOT_ORE: usize = 1;
const B_OBBI_BOT_ORE: usize = 2;
const B_OBBI_BOT_CLAY: usize = 3;
const B_GEODE_BOT_ORE: usize = 4;
const B_GEODE_BOT_OBBI: usize = 5;

type Blueprint = [i64; 6];

const N_ORE: usize = 0;
const N_ORE_BOTS: usize = 1;
const N_CLAY: usize = 2;
const N_CLAY_BOTS: usize = 3;
const N_OBBI: usize = 4;
const N_OBBI_BOTS: usize = 5;
const N_GEODE: usize = 6;
const N_GEODE_BOTS: usize = 7;

type Node = [i64; 8];

fn add_nodes(b: [i64; 8], d: [i64; 8]) -> [i64; 8] {
  b.into_iter().zip(d.into_iter()).map(|v| v.0 + v.1).collect::<Vec<_>>().try_into().unwrap()
}

#[derive(PartialEq, Copy, Clone)]
struct ScoredNode(pub Node, pub f64);

impl Eq for ScoredNode { }

impl Ord for ScoredNode {
  fn cmp(&self, other: &Self) -> Ordering {
    match self.partial_cmp(other) {
      None => Ordering::Equal,
      Some(ord) => ord
    }
  }
}

impl PartialOrd for ScoredNode {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    other.1.partial_cmp(&self.1)
  }
}

// all times are based on the first blueprint in the example data, when searching for 9 geodes
mod heuristics {
  use crate::s36::{N_GEODE, N_GEODE_BOTS, Node};

  pub fn default(node: Node, dest_geodes: i64) -> f64 {
    projected_geodes(node, dest_geodes)
    //projected_geodes(node, dest_geodes)
  }

  // dumb heuristic: 14.34s
  pub fn dumb() -> i64 {
    0
  }

  // projected geodes heuristic: 0.43s
  pub fn projected_geodes(node: Node, dest_geodes: i64) -> f64 {
    // geodes = (num_bots + t / 2) * t
    // 0 = (1/2)*t^2 + num_bots*t - geodes
    // t = sqrt(num_bots^2 + 2*geodes)-num_bots
    let needed_geodes = dest_geodes - node[N_GEODE];
    ((node[N_GEODE_BOTS].pow(2) + 2 * needed_geodes) as f64).sqrt() - node[N_GEODE_BOTS] as f64
  }
}

fn a_star(blueprint: Blueprint, start_node: Node, end_geodes: i64) -> i64 {
  let mut pq = BinaryHeap::new();
  pq.push(ScoredNode(start_node, 0.0));
  let mut g_score = HashMap::new();
  let mut prev = HashMap::new();
  g_score.insert(start_node, 0);
  while !pq.is_empty() {
    let scored_node = pq.pop().unwrap();
    let node = scored_node.0;
    if node[N_GEODE] >= end_geodes {
      // found end
      let mut track_node = scored_node.0;
      if OUTPUT_PATHS {
        let mut path = vec![];
        while track_node != start_node {
          path.push(track_node);
          track_node = prev[&track_node];
        }
        path.push(track_node);
        path.reverse();
        path.iter().enumerate().for_each(|v| {
          println!("{} {:?}", v.0, v.1);
        });
      }
      return g_score[&scored_node.0];
    }
    vec![
      // make nothing
      node,
      // make ore bot
      add_nodes(node, [-blueprint[B_ORE_BOT_ORE], 1, 0, 0, 0, 0, 0, 0]),
      // make clay bot
      add_nodes(node, [-blueprint[B_CLAY_BOT_ORE], 0, 0, 1, 0, 0, 0, 0]),
      // make obbi bot
      add_nodes(node, [-blueprint[B_OBBI_BOT_ORE], 0, -blueprint[B_OBBI_BOT_CLAY], 0, 0, 1, 0, 0]),
      // make geobe bot
      add_nodes(node, [-blueprint[B_GEODE_BOT_ORE], 0, 0, 0, -blueprint[B_GEODE_BOT_OBBI], 0, 0, 1]),
    ].into_iter()
      .filter(|v| v.iter().all(|v| *v >= 0))
      .map(|v| add_nodes(v, [node[N_ORE_BOTS], 0, node[N_CLAY_BOTS], 0, node[N_OBBI_BOTS], 0, node[N_GEODE_BOTS], 0]))
      .for_each(|v| {
        let maybe_g_score = g_score[&scored_node.0] + 1;
        if !g_score.contains_key(&v) || g_score[&v] > maybe_g_score {
          g_score.insert(v, maybe_g_score);
          if OUTPUT_PATHS {
            prev.insert(v, scored_node.0);
          }
          let h_score = heuristics::default(v, end_geodes);
          if h_score.is_finite() {
            pq.push(ScoredNode(v, maybe_g_score as f64 + h_score));
          }
        }
      });
  }
  panic!("destination unreachable")
}

fn thread_main(blueprint: Blueprint) -> i64 {
  let mut upper = i64::MAX;
  let mut lower = 0i64;
  // discover upper bound
  let mut tentative_upper_bound = 16i64;
  loop {
    let time = a_star(blueprint, [0, 1, 0, 0, 0, 0, 0, 0], tentative_upper_bound);
    if time > MAX_TIME {
      upper = tentative_upper_bound;
      break;
    } else {
      lower = tentative_upper_bound;
      tentative_upper_bound *= 4;
    }
  }
  // bsearch
  todo!();
  while upper > lower {
    let mid = (upper + lower + 1) / 2;
    let length = a_star(blueprint, [0, 1, 0, 0, 0, 0, 0, 0], mid);
    if length > MAX_TIME {
      // cannot make n geodes in time
      upper = mid;
    } else {
      lower = mid;
    }
  }
  lower
}

fn real_main() {
  let ans = input().split_str("\n")
    .map(|v| v.split_str(" ")
      .map(|v| v.to_str_lossy().parse::<i64>())
      .collect::<Vec<_>>())
    .map(|v| [*v[6].as_ref().unwrap(), *v[12].as_ref().unwrap(), *v[18].as_ref().unwrap(), *v[21].as_ref().unwrap(), *v[27].as_ref().unwrap(), *v[30].as_ref().unwrap()])
    //.par_bridge()
    .map(|v| thread_main(v))
    .collect::<Vec<_>>()
    .into_iter()
    .enumerate()
    .map(|v| (v.0 + 1) as i64 * v.1)
    .sum::<i64>();
  println!("{}", ans);
}

fn test_main() {
  let input = input().split_str("\n")
    .map(|v| v.split_str(" ")
      .map(|v| v.to_str_lossy().parse::<i64>())
      .collect::<Vec<_>>())
    .map(|v| [*v[6].as_ref().unwrap(), *v[12].as_ref().unwrap(), *v[18].as_ref().unwrap(), *v[21].as_ref().unwrap(), *v[27].as_ref().unwrap(), *v[30].as_ref().unwrap()])
    .collect::<Vec<_>>();
  println!("{}", a_star(input[0], [0, 1, 0, 0, 0, 0, 0, 0], 9));
}

pub fn main() {
  test_main();
}