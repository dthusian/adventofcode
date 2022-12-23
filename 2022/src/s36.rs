use std::cmp::Ordering;
use std::collections::{BinaryHeap, BTreeSet, HashMap, VecDeque};
use bstr::ByteSlice;
use indicatif::ParallelProgressIterator;
use rayon::prelude::*;
use crate::util::input;

const DEBUG: bool = false;

const MAX_TIME: i16 = 24;

const B_ORE_BOT_ORE: usize = 0;
const B_CLAY_BOT_ORE: usize = 1;
const B_OBBI_BOT_ORE: usize = 2;
const B_OBBI_BOT_CLAY: usize = 3;
const B_GEODE_BOT_ORE: usize = 4;
const B_GEODE_BOT_OBBI: usize = 5;

type Blueprint = [i16; 6];

const N_ORE: usize = 0;
const N_ORE_BOTS: usize = 1;
const N_CLAY: usize = 2;
const N_CLAY_BOTS: usize = 3;
const N_OBBI: usize = 4;
const N_OBBI_BOTS: usize = 5;
const N_GEODE: usize = 6;
const N_GEODE_BOTS: usize = 7;

type Node = [i16; 8];

const START_NODE: Node = [0, 1, 0, 0, 0, 0, 0, 0];

fn add_nodes(b: Node, d: Node) -> Node {
  b.into_iter().zip(d.into_iter()).map(|v| v.0 + v.1).collect::<Vec<_>>().try_into().unwrap()
}

#[derive(PartialEq, Copy, Clone)]
struct ScoredNode(pub Node, pub f32);

impl Eq for ScoredNode { }

impl Ord for ScoredNode {
  // NaN is greater than everything to not search those nodes
  fn cmp(&self, other: &Self) -> Ordering {
    other.1.partial_cmp(&self.1).unwrap()
  }
}

impl PartialOrd for ScoredNode {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

// all times are based on the first blueprint in the example data, when searching for 9 geodes
mod heuristics {
  use crate::s36::{N_GEODE, N_GEODE_BOTS, Node};

  pub fn default(node: Node, dest_geodes: i16) -> f32 {
    projected_geodes(node, dest_geodes)
  }

  pub fn dumb() -> f64 {
    0.0
  }

  pub fn projected_geodes(node: Node, dest_geodes: i16) -> f32 {
    // geodes = (num_bots + t / 2) * t
    // 0 = (1/2)*t^2 + num_bots*t - geodes
    // t = sqrt(num_bots^2 + 2*geodes)-num_bots
    let needed_geodes = dest_geodes - node[N_GEODE];
    ((node[N_GEODE_BOTS].pow(2) + 2 * needed_geodes) as f32).sqrt() - node[N_GEODE_BOTS] as f32
  }
}

fn a_star(blueprint: Blueprint, start_node: Node, end_geodes: i16) -> i16 {
  let mut pq = BinaryHeap::new();
  pq.push(ScoredNode(start_node, 0.0));
  let mut g_score = HashMap::<Node, i16>::new();
  let mut prev = HashMap::new();
  g_score.insert(start_node, 0);
  while !pq.is_empty() {
    let scored_node = pq.pop().unwrap();
    let node = scored_node.0;
    if node[N_GEODE] >= end_geodes {
      // found end
      let total_dist = g_score[&node];
      let mut track_node = node;
      if DEBUG {
        let mut path = vec![];
        while track_node != start_node {
          path.push(track_node);
          track_node = prev[&track_node];
        }
        path.push(track_node);
        path.reverse();
        path.iter().enumerate().for_each(|v| {
          println!("{} {:>3?}", v.0, v.1);
        });
      }
      return total_dist;
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
        let maybe_g_score = g_score[&node] + 1;
        if !g_score.contains_key(&v) || g_score[&v] > maybe_g_score {
          g_score.insert(v, maybe_g_score);
          if DEBUG {
            prev.insert(v, node);
          }
          let h_score = heuristics::default(v, end_geodes);
          if DEBUG {
            if h_score.is_nan() {
              panic!("Heuristic returned NaN");
            }
          }
          pq.push(ScoredNode(v, maybe_g_score as f32 + h_score));
        }
      });
  }
  panic!("destination unreachable")
}

fn thread_main(blueprint: Blueprint) -> i16 {
  let mut upper;
  let mut lower = 0;
  // discover upper bound
  let mut tentative_upper_bound = 16;
  loop {
    let time = a_star(blueprint, START_NODE, tentative_upper_bound);
    if time > MAX_TIME {
      upper = tentative_upper_bound;
      break;
    } else {
      lower = tentative_upper_bound;
      tentative_upper_bound *= 4;
    }
  }
  // bsearch
  while upper - lower > 1 {
    let mid = (upper + lower + 1) / 2;
    let length = a_star(blueprint, START_NODE, mid);
    if length > MAX_TIME {
      // cannot make n geodes in time
      upper = mid;
    } else {
      lower = mid;
    }
  }
  lower
}

fn parse_input() -> Vec<Blueprint> {
  input().split_str("\n")
    .map(|v| v.split_str(" ")
      .map(|v| v.to_str_lossy().parse::<i16>())
      .collect::<Vec<_>>())
    .map(|v| [*v[6].as_ref().unwrap(), *v[12].as_ref().unwrap(), *v[18].as_ref().unwrap(), *v[21].as_ref().unwrap(), *v[27].as_ref().unwrap(), *v[30].as_ref().unwrap()])
    .collect::<Vec<_>>()
}

fn real_main() {
  let ans = parse_input()
    .into_par_iter()
    .progress()
    .map(|v| thread_main(v))
    .collect::<Vec<_>>()
    .into_iter()
    .enumerate()
    .map(|v| (v.0 + 1) as i64 * v.1 as i64)
    .sum::<i64>();
  println!("{}", ans);
}

pub fn main() {
  real_main()
}