use std::cmp::Ordering;
use std::collections::{BinaryHeap, BTreeSet, HashMap, VecDeque};
use bstr::ByteSlice;
use indicatif::ParallelProgressIterator;
use rayon::prelude::*;
use crate::util::input;

const OUTPUT_PATHS: bool = false;

const MAX_TIME: i64 = 32;

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

const START_NODE: Node = [0, 1, 0, 0, 0, 0, 0, 0];

fn add_nodes(b: Node, d: Node) -> Node {
  b.into_iter().zip(d.into_iter()).map(|v| v.0 + v.1).collect::<Vec<_>>().try_into().unwrap()
}

#[derive(PartialEq, Eq, Copy, Clone)]
struct ScoredNode(pub Node, pub i64);

impl Ord for ScoredNode {
  // NaN is greater than everything to not search those nodes
  fn cmp(&self, other: &Self) -> Ordering {
    other.1.cmp(&self.1)
  }
}

impl PartialOrd for ScoredNode {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

// all times are based on the first blueprint in the example data, when searching for 9 geodes
mod heuristics {
  use crate::s37::{N_GEODE, N_GEODE_BOTS, Node};

  pub fn default(node: Node, dest_geodes: i64) -> i64 {
    projected_geodes(node, dest_geodes)
  }

  pub fn dumb() -> i64 {
    0
  }

  pub fn projected_geodes(node: Node, dest_geodes: i64) -> i64 {
    // geodes = (num_bots + t / 2) * t
    // 0 = (1/2)*t^2 + num_bots*t - geodes
    // t = sqrt(num_bots^2 + 2*geodes)-num_bots
    let needed_geodes = dest_geodes - node[N_GEODE];
    ((node[N_GEODE_BOTS].pow(2) + 2 * needed_geodes) as f64).sqrt().ceil() as i64 - node[N_GEODE_BOTS]
  }
}

fn a_star(blueprint: Blueprint, start_node: Node, end_geodes: i64) -> i64 {
  let mut pq = BinaryHeap::new();
  pq.push(ScoredNode(start_node, 0));
  let mut g_score = HashMap::<Node, i64>::new();
  let mut prev = HashMap::new();
  g_score.insert(start_node, 0);
  while !pq.is_empty() {
    let scored_node = pq.pop().unwrap();
    let node = scored_node.0;
    if node[N_GEODE] >= end_geodes {
      // found end
      let total_dist = g_score[&node];
      let mut track_node = node;
      if OUTPUT_PATHS {
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
          if OUTPUT_PATHS {
            prev.insert(v, node);
          }
          let h_score = heuristics::default(v, end_geodes);
          pq.push(ScoredNode(v, maybe_g_score + h_score));
        }
      });
  }
  panic!("destination unreachable")
}

fn thread_main(blueprint: Blueprint) -> i64 {
  let mut upper;
  let mut lower = 0i64;
  // discover upper bound
  let mut tentative_upper_bound = 16i64;
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
      .map(|v| v.to_str_lossy().parse::<i64>())
      .collect::<Vec<_>>())
    .map(|v| [*v[6].as_ref().unwrap(), *v[12].as_ref().unwrap(), *v[18].as_ref().unwrap(), *v[21].as_ref().unwrap(), *v[27].as_ref().unwrap(), *v[30].as_ref().unwrap()])
    .collect::<Vec<_>>()
}

fn real_main() {
  let ans = parse_input()
    .into_iter()
    .take(3)
    .par_bridge()
    .map(|v| thread_main(v))
    .collect::<Vec<_>>()
    .into_iter()
    .product::<i64>();
  println!("{}", ans);
}

pub fn main() {
  real_main()
}