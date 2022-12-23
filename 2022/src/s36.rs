use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::env;
use std::fs::File;
use std::io::{Read, Write};
use bstr::ByteSlice;
use indicatif::ParallelProgressIterator;
use rayon::prelude::*;
use crate::util::input;

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

#[derive(Copy, Clone, Default, Debug)]
struct AStarConfig {
  pub debug: bool,
  pub extract_nodes: bool
}

// all times are based on the first blueprint in the example data, when searching for 9 geodes
mod heuristics {
  use num::Zero;
  use num_complex::{Complex32, ComplexFloat};
  use crate::s36::{B_GEODE_BOT_OBBI, Blueprint, N_GEODE, N_GEODE_BOTS, N_OBBI, N_OBBI_BOTS, Node};

  pub fn default(blueprint: Blueprint, node: Node, dest_geodes: i16) -> f32 {
    projected_resources_2(blueprint, node, dest_geodes)
  }

  pub fn dumb() -> f32 {
    0.0
  }

  pub fn projected_geodes(_: Blueprint, node: Node, dest_geodes: i16) -> f32 {
    // geodes = (num_bots + t / 2) * t
    // 0 = (1/2)*t^2 + num_bots*t - geodes
    // t = sqrt(num_bots^2 + 2*geodes)-num_bots
    let needed_geodes = dest_geodes - node[N_GEODE];
    ((node[N_GEODE_BOTS].pow(2) + 2 * needed_geodes) as f32).sqrt() - node[N_GEODE_BOTS] as f32
  }

  pub fn projected_resources_2(blueprint: Blueprint, node: Node, dest_geodes: i16) -> f32 {
    if node[N_GEODE] >= dest_geodes {
      return 0.0
    }
    let a = blueprint[B_GEODE_BOT_OBBI] as f32 / 4.0;
    let b = (blueprint[B_GEODE_BOT_OBBI] as f32) * (node[N_OBBI_BOTS] as f32) / 2.0;
    let c = node[N_GEODE_BOTS] as f32 + (blueprint[B_GEODE_BOT_OBBI] as f32) * (node[N_OBBI] as f32) / 2.0;
    let d = dest_geodes as f32 - node[N_GEODE] as f32;
    let delta0 = Complex32::new(b * b - 3.0 * a * c, 0.0);
    let delta1 = Complex32::new(2.0 * b * b * b - 9.0 * a * b * c + 27.0 * a * a * d, 0.0);
    let big_c_0 = ((delta1 + (delta1 * delta1 - 4.0 * delta0 * delta0 * delta0).sqrt()) / 2.0).cbrt();
    let big_c_1 = ((delta1 - (delta1 * delta1 - 4.0 * delta0 * delta0 * delta0).sqrt()) / 2.0).cbrt();
    let big_c;
    let delta0_over_big_c;
    if big_c_0.is_zero() && big_c_1.is_zero() {
      big_c = Complex32::zero();
      delta0_over_big_c = Complex32::zero();
    } else if big_c_0.is_zero() {
      big_c = big_c_1;
      delta0_over_big_c = delta0 / big_c;
    } else {
      big_c = big_c_0;
      delta0_over_big_c = delta0 / big_c;
    }
    let cbrt_unity_1 = Complex32::new(-0.5, 3.0f32.sqrt() / 2.0);
    let cbrt_unity_2 = Complex32::new(-0.5, -3.0f32.sqrt() / 2.0);
    let root_0 = -(b + big_c + delta0_over_big_c) / (3.0 * a);
    let root_1 = -(b + cbrt_unity_1 * big_c + cbrt_unity_1.recip() * delta0_over_big_c) / (3.0 * a);
    let root_2 = -(b + cbrt_unity_2 * big_c + cbrt_unity_2.recip() * delta0_over_big_c) / (3.0 * a);
    if root_0.im < 1e-5 {
      root_0.re
    } else if root_1.im < 1e-5 {
      root_1.re
    } else if root_2.im < 1e-5 {
      root_2.re
    } else {
      f32::INFINITY
    }
  }
}

fn a_star<F: Fn(Blueprint, Node, i16) -> f32>(blueprint: Blueprint, start_node: Node, end_geodes: i16, heuristic: F, config: AStarConfig) -> i16 {
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
      if config.extract_nodes {
        let mut f = File::create("debug.json").unwrap();
        f.write(&serde_json::to_vec(&g_score.keys().collect::<Vec<_>>()).unwrap()).unwrap();
        println!("{} nodes written to file", g_score.len());
      }
      if config.debug {
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
          let h_score = heuristic(blueprint, v, end_geodes);
          g_score.insert(v, maybe_g_score);
          if config.debug {
            prev.insert(v, node);
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
    let time = a_star(blueprint, START_NODE, tentative_upper_bound, heuristics::default, AStarConfig::default());
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
    let length = a_star(blueprint, START_NODE, mid, heuristics::default, AStarConfig::default());
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

fn test_main() {
  let blueprint = [4, 2, 3, 14, 2, 7];
  println!("{}",
    a_star(
      blueprint,
      START_NODE,
      9,
      heuristics::default,
      AStarConfig {
        debug: false,
        extract_nodes: true
      }
    )
  );
}

fn compliance_test() {
  let blueprint = [4, 2, 3, 14, 2, 7];
  let mut buf = vec![];
  File::open("debug.json").unwrap().read_to_end(&mut buf).unwrap();
  let nodes = serde_json::from_slice::<Vec<Node>>(&buf).unwrap();
  nodes.into_par_iter()
    .progress()
    .for_each(|v| {
    let real_dist = a_star(blueprint, v, 9, heuristics::projected_geodes, AStarConfig::default());
    let h_score = heuristics::projected_resources_2(blueprint, v, 9);
    if h_score > real_dist as f32 {
      println!("node {:?} h={} real={}", v, h_score, real_dist);
    }
  });
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
  let mode = env::var("AOC_MODE").unwrap_or("default".into());
  if mode == "compliance" {
    compliance_test();
  } else if mode == "write_nodes" {
    test_main();
  } else {
    real_main();
  }
}