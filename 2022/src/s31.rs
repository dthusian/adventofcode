use std::cmp::{max, min};
use std::collections::HashMap;
use std::iter::Iterator;
use bstr::{BString, ByteSlice};
use indicatif::{ParallelProgressIterator, ProgressBar};
use itermore::IterSorted;
use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelBridge, ParallelIterator};
use rayon::range::Iter;
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

type OptimizedGraph = (Vec<i64> /* valve flow rates */, HashMap<(i64, i64), i64> /* dist */, Vec<i64> /* distance from starting node */);

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
  let mut dist2 = HashMap::new();
  let dist2aa = relevant.iter().map(|v| dist[&(chars_to_node_id(b"AA"), *v)]).collect::<Vec<_>>();
  let value2 = relevant.iter().map(|v| value[v]).collect::<Vec<_>>();
  for rnode_i in relevant.iter().enumerate() {
    for rnode_j in relevant.iter().enumerate() {
      dist2.insert((rnode_i.0 as i64, rnode_j.0 as i64), dist[&(*rnode_i.1, *rnode_j.1)]);
    }
  }
  (value2, dist2, dist2aa)
}

fn bitmask_to_flow(bitmask: u64, flows: &[i64]) -> i64 {
  flows.iter()
    .enumerate()
    .filter(|v| bitmask & (1 << v.0) != 0)
    .map(|v| *v.1)
    .sum()
}

type DPElement = i64;
type DPState = Vec<Vec<Vec<Option<DPElement>>>>;

fn max_assign<T: Ord + Clone>(a: &mut T, b: T) {
  *a = max((*a).clone(), b);
}

fn dp(flows: &Vec<i64>, dist: &HashMap<(i64, i64), i64>, dist2aa: &Vec<i64>, mask: u64) -> i64 {
  let num_nodes = flows.len();
  let mut state: DPState = vec![vec![vec![None; num_nodes]; 1 << num_nodes]; 35];
  // initialize base case
  for node in 0..num_nodes {
    state[dist2aa[node] as usize][0][node] = Some(0);
  }
  // dp dp dp dp dp dp
  let mut best_pressure = i64::MIN;
  for t in 1..=26 {
    for omask in 0..1 << num_nodes {
      for pos in 0..num_nodes {
        let current = state[t][omask][pos];
        if current.is_none() { continue; }
        let current = current.unwrap();
        // possible move: wait one minute
        max_assign(&mut state[t + 1][omask][pos],
                   Some(current + bitmask_to_flow(omask as u64 & mask, &flows)));
        // possible move: move to a node
        for possible_move in 0..num_nodes {
          let dist_to_dest = dist[&(pos as i64, possible_move as i64)];
          if dist_to_dest + t as i64 > 30 {
            // cannot reach in time
            continue;
          }
          max_assign(&mut state[t + dist_to_dest as usize][omask][possible_move],
                     Some(current + bitmask_to_flow(omask as u64 & mask, &flows) * dist_to_dest));
        }
        // possible move: open the current valve
        max_assign(&mut state[t + 1][omask | (1 << pos)][pos],
                   Some(current + bitmask_to_flow(omask as u64 & mask, &flows)));
        best_pressure = max(best_pressure, current);
      }
    }
  }
  best_pressure
}

pub fn main() {
  let old_graph = parse(&input());
  let (flows, dist, dist2aa) = optimize_graph(&old_graph.0, &old_graph.1);
  let num_nodes = flows.len();
  if num_nodes > 63 {
    panic!("bitmask cannot store more than 63 nodes");
  }
  let num_masks = 1 << (num_nodes - 1);
  let best_pressure = (0u64..num_masks)
    .into_par_iter()
    .progress_count(num_masks)
    .map(|rmask| dp(&flows, &dist, &dist2aa, rmask) + dp(&flows, &dist, &dist2aa, !rmask))
    .reduce(|| i64::MIN,|a, b| max(a, b));
  println!("{}", best_pressure);
}

// rmask = role mask, omask = opened mask