use std::cmp::{max, min};
use std::collections::HashMap;
use bstr::{BString, ByteSlice};
use itermore::IterSorted;
use crate::util::input;

fn find<T: IntoIterator>(iter: T, element: <T as IntoIterator>::Item) -> usize
  where <T as IntoIterator>::Item: PartialEq
{
  iter.into_iter().position(|v| v == element).unwrap()
}

fn chars_to_node_id(chrs: &[u8]) -> u16 {
  u16::from_be_bytes(chrs.try_into().unwrap())
}

fn parse(input: &BString) -> (HashMap<u16, i64>, HashMap<u16, Vec<u16>>) {
  let mut value = HashMap::new();
  let mut adj = HashMap::new();
  input.split_str("\n").for_each(|v| {
    let spl = v.split_str(" ").collect::<Vec<_>>();
    let id = chars_to_node_id(spl[1]);
    let flow = v[v.find("=").unwrap() + 1 .. v.find(";").unwrap()].to_str_lossy().parse().unwrap();
    value.insert(id, flow);
    let mut edges_start = v.find("valve").unwrap() + 5;
    if v[edges_start] == b's' {
      edges_start += 2;
    } else {
      edges_start += 1;
    }
    adj.insert(id, vec![]);
    v[edges_start..].split_str(", ")
      .for_each(|v| {
        adj.get_mut(&id).unwrap().push(chars_to_node_id(v));
      });
  });
  (value, adj)
}

type OptimizedGraph = (HashMap<i64, i64> /* valve flow rates */, HashMap<(i64, i64), i64> /* dist */, Vec<i64> /* distance from starting node */);

fn optimize_graph(value: &HashMap<u16, i64>, adj: &HashMap<u16, Vec<u16>>) -> OptimizedGraph {
  let node_names = value.keys().map(|v| *v).collect::<Vec<_>>();
  let mut dist = HashMap::new();
  // Floyd-Warshall
  for i in &node_names {
    for j in &node_names {
      dist.insert((*i, *j), 99999);
    }
  }
  for i in &node_names {
    *dist.get_mut(&(*i, *i)).unwrap() = 0;
    for n in &adj[&i] {
      *dist.get_mut(&(*i, *n)).unwrap() = 1;
    }
  }
  for k in &node_names {
    for i in &node_names {
      for j in &node_names {
        if dist[&(*i, *j)] > dist[&(*i, *k)] + dist[&(*k, *j)] {
          *dist.get_mut(&(*i, *j)).unwrap() = dist[&(*i, *k)] + dist[&(*k, *j)];
        }
      }
    }
  }
  let relevant = value.iter().filter(|v| *v.1 > 0).map(|v| *v.0).sorted().collect::<Vec<_>>();
  println!("{:?}", relevant.iter().map(|v| v.to_be_bytes().to_str_lossy().into_owned()).collect::<Vec<_>>());
  let mut value2 = HashMap::new();
  let mut dist2 = HashMap::new();
  let dist2aa = relevant.iter().map(|v| dist[&(chars_to_node_id(b"AA"), *v)]).collect::<Vec<_>>();
  for rnode in relevant.iter().enumerate() {
    value2.insert(rnode.0 as i64, value[rnode.1]);
  }
  for rnode_i in relevant.iter().enumerate() {
    for rnode_j in relevant.iter().enumerate() {
      dist2.insert((rnode_i.0 as i64, rnode_j.0 as i64), dist[&(*rnode_i.1, *rnode_j.1)]);
    }
  }
  (value2, dist2, dist2aa)
}

fn bitmask_to_flow(bitmask: u64, graph: &OptimizedGraph) -> i64 {
  graph.0.iter()
    .filter(|v| bitmask & (1 << *v.0) != 0)
    .map(|v| *v.1)
    .sum()
}

type TrackingElement = String;
type DPElement = (i64, Vec<TrackingElement>);
type DPState = Vec<Vec<Vec<Option<DPElement>>>>;

// returns maximum possible pressure release given current state (time is at the end of the minute)
fn dp(state: &mut DPState, graph: &OptimizedGraph, time: i64, bitmask: u64, position: i64) -> DPElement {
  if let Some(cached) = state[time as usize][bitmask as usize][position as usize].clone() {
    // memoize
    cached
  } else {
    // base case
    if time < graph.2[position as usize] {
      return (-999, vec![]);
    } else if time == graph.2[position as usize] {
      if bitmask == 0 {
        return (0, vec![format!("dp({}, {}, {}) -> 0", time, bitmask, position)]);
      } else {
        return (-999, vec![]);
      }
    }
    // compute
    let possible_moves = (0..graph.0.len() as i64)
      // Filter only for moves that lead to this state (top-down)
      .filter(|v| (bitmask & (1 << *v)) != 0)
      .collect::<Vec<_>>();
    // fallback case: waste a minute and do nothing
    let (mut best_pressure, mut best_pressure_path) = dp(state, graph, time - 1, bitmask, position);
    best_pressure += bitmask_to_flow(bitmask, graph);
    for cmove in possible_moves {
      let time_to_prev = graph.1[&(position, cmove)] + 1;
      // can't get to this node in time
      if time - time_to_prev <= 0 {
        continue;
      }
      let prev_bitmask = bitmask & !(1 << cmove);
      let (mut maybe_best_pressure, maybe_best_pressure_path) = dp(state, graph, time - time_to_prev, prev_bitmask, cmove);
      if maybe_best_pressure < 0 {
        continue;
      }
      maybe_best_pressure += bitmask_to_flow(prev_bitmask, graph) * time_to_prev;
      if maybe_best_pressure > best_pressure {
        best_pressure = maybe_best_pressure;
        best_pressure_path = maybe_best_pressure_path;
      }
    }
    best_pressure_path.push(format!("dp({}, {:b}, {}) -> {}", time, bitmask, position, best_pressure));
    state[time as usize][bitmask as usize][position as usize] = Some((best_pressure, best_pressure_path.clone()));
    (best_pressure, best_pressure_path)
  }
}

pub fn main() {
  let old_graph = parse(&input());
  let graph = optimize_graph(&old_graph.0, &old_graph.1);
  if graph.0.len() > 63 {
    panic!("bitmask cannot store more than 63 nodes");
  }
  for i in 0..graph.0.len() as i64 {
    println!("{}", (0..graph.0.len() as i64).map(|j| graph.1[&(i, j)].to_string()).collect::<Vec<_>>().join("\t"));
  }
  let mut state: DPState = vec![vec![vec![None; graph.0.len()]; 1 << graph.0.len()]; 32];
  let mut best_pressure = i64::MIN;
  let mut best_pressure_path = vec![];
  for mask in 0u64.. 1 << graph.0.len() {
    for final_position in 0..graph.0.len() {
      let (maybe_best_pressure, maybe_best_pressure_path) = dp(&mut state, &graph, 30, mask, final_position as i64);
      if maybe_best_pressure > best_pressure {
        best_pressure = maybe_best_pressure;
        best_pressure_path = maybe_best_pressure_path;
      }
    }
  }
  println!("{}", best_pressure);
  println!("{:?}", best_pressure_path);
  println!("{:?}", dp(&mut state, &graph, 30, 0b111111, 1));
}