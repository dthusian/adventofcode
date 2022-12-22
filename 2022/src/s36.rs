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

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
enum Node {
  Normal([i64; 8]),
  MetaGeode(i64)
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
struct ScoredNode(pub Node, pub i64);

impl Ord for ScoredNode {
  fn cmp(&self, other: &Self) -> Ordering {
    other.1.cmp(&self.1)
  }
}

impl PartialOrd for ScoredNode {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    other.1.partial_cmp(&self.1)
  }
}

// technically non-admissible but still OK due to knowledge of a property of the graph
fn heuristic(node: Node, destination: Node) -> i64 {
  if node == destination {
    return 0;
  }
  let dest_geodes = match destination {
    Node::Normal(_) => panic!("heuristic can only model distance to geodes"),
    Node::MetaGeode(geodes) => geodes
  };
  if let Node::Normal(node) = node {
    return 0;
    // geodes = (num_bots + t / 2) * t
    // 0 = (1/2)*t^2 + num_bots*t - geodes
    // t = sqrt(num_bots^2 + 2*geodes)-num_bots
    let needed_geodes = dest_geodes - node[N_GEODE];
    ((node[N_GEODE_BOTS].pow(2) + 2 * needed_geodes) as f64).sqrt().round() as i64 - node[N_GEODE_BOTS]
    //needed_geodes / (1 + node[N_ORE_BOTS] / 2 + node[N_CLAY_BOTS] / 2 + node[N_OBBI_BOTS] + node[N_GEODE_BOTS])
  } else {
    i64::MAX
  }
}

fn a_star(blueprint: Blueprint, start_node: Node, end_node: Node) -> i64 {
  fn util(b: [i64; 8], d: [i64; 8]) -> [i64; 8] {
    b.into_iter().zip(d.into_iter()).map(|v| v.0 + v.1).collect::<Vec<_>>().try_into().unwrap()
  }
  let mut pq = BinaryHeap::new();
  pq.push(ScoredNode(start_node, 0));
  let mut g_score = HashMap::new();
  let mut prev = HashMap::new();
  g_score.insert(start_node, 0);
  while !pq.is_empty() {
    let node2 = pq.pop().unwrap();
    if let Node::Normal(node) = node2.0 {
      vec![
        // make nothing
        Node::Normal(node),
        // make ore bot
        Node::Normal(util(node, [-blueprint[B_ORE_BOT_ORE], 1, 0, 0, 0, 0, 0, 0])),
        // make clay bot
        Node::Normal(util(node, [-blueprint[B_CLAY_BOT_ORE], 0, 0, 1, 0, 0, 0, 0])),
        // make obbi bot
        Node::Normal(util(node, [-blueprint[B_OBBI_BOT_ORE], 0, -blueprint[B_OBBI_BOT_CLAY], 0, 0, 1, 0, 0])),
        // make geobe bot
        Node::Normal(util(node, [-blueprint[B_GEODE_BOT_ORE], 0, 0, 0, -blueprint[B_GEODE_BOT_OBBI], 0, 0, 1])),
        // corresponding geode metanode
        Node::MetaGeode(node[N_GEODE])
      ].into_iter()
        .filter(|v| match v {
          Node::Normal(state) => state.iter().all(|v| *v >= 0),
          Node::MetaGeode(_) => true
        })
        .map(|v| match v {
          Node::Normal(state) => Node::Normal(util(state, [node[N_ORE_BOTS], 0, node[N_CLAY_BOTS], 0, node[N_OBBI_BOTS], 0, node[N_GEODE_BOTS], 0])),
          Node::MetaGeode(_) => v
        })
        .for_each(|v| {
          let maybe_g_score = g_score[&node2.0] + 1;
          if !g_score.contains_key(&v) || g_score[&v] > maybe_g_score {
            g_score.insert(v, maybe_g_score);
            if OUTPUT_PATHS {
              prev.insert(v, node2.0);
            }
            let h_score = heuristic(v, end_node);
            if h_score < i64::MAX {
              pq.push(ScoredNode(v, maybe_g_score + h_score));
            }
          }
        });
    } else if node2.0 == end_node {
      let mut track_node = end_node;
      let mut path = vec![];
      while track_node != start_node {
        track_node = prev[&track_node];
        path.push(track_node);
      }
      path.reverse();
      path.iter().for_each(|v| {
        println!("{:?}", v);
      });
      return g_score[&node2.0] - 1;
    }
  }
  panic!("destination unreachable")
}

fn thread_main(blueprint: Blueprint) -> i64 {
  let mut upper = 100;
  let mut lower = 0;
  while upper != lower {
    let mid = (upper + lower) / 2;
    let length = a_star(blueprint, Node::Normal([0, 1, 0, 0, 0, 0, 0, 0]), Node::MetaGeode(mid));
    if length > MAX_TIME {
      // cannot make n geodes in time
      upper = mid - 1;
    } else {
      lower = mid;
    }
  }
  upper
}

pub fn real_main() {
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
  println!("{}", a_star(input[0], Node::Normal([0, 1, 0, 0, 0, 0, 0, 0]), Node::MetaGeode(10)));
}

pub fn main() {
  test_main();
}