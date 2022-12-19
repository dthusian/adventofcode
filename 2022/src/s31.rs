use std::cmp::{max, min, Ordering};
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

type OptimizedGraph = (Vec<i64> /* valve flow rates */, HashMap<(usize, usize), bool> /* adj */);

fn optimize_graph(value: &HashMap<u16, i64>, adj: &HashMap<u16, Vec<u16>>) -> OptimizedGraph {
  let remap = value.iter().map(|v| *v.0).sorted_by(|a, b| {
    if *a == chars_to_node_id(b"AA") {
      return Ordering::Less
    }
    if *b == chars_to_node_id(b"AA") {
      return Ordering::Greater
    }
    value[b].cmp(&value[a])
  }).collect::<Vec<_>>();
  let mut adj2 = HashMap::<(usize, usize), bool>::new();
  for i in 0..adj.len() {
    for j in 0..adj.len() {
      adj2.insert((i, j), false);
    }
  }
  for i in 0..adj.len() {
    let adjacent = &adj[&remap[i]];
    for j in adjacent {
      *adj2.get_mut(&(i, remap.iter().position(|v| v == j).unwrap())).unwrap() = true;
    }
  }
  let mut value2 = remap.iter().map(|v| value[v]).collect::<Vec<_>>();
  (value2, adj2)
}

fn bitmask_to_flow(bitmask: u64, flows: &[i64]) -> i64 {
  flows.iter()
    .enumerate()
    .filter(|v| bitmask & (1 << v.0) != 0)
    .map(|v| *v.1)
    .sum()
}

fn max_assign<T: Ord + Clone>(a: &mut T, b: T) {
  *a = max((*a).clone(), b);
}

pub fn main() {
  let old_graph = parse(&input());
  let (flows, adj) = optimize_graph(&old_graph.0, &old_graph.1);
  let num_nodes = flows.len();
  let num_useful_valves = flows.iter().enumerate().filter(|v| *v.1 != 0).last().unwrap().0 + 1;
  println!("num nodes: {}", num_nodes);
  println!("num nodes with flowrate!=0: {}", num_useful_valves);
  if num_useful_valves > 63 {
    panic!("bitmask cannot store more than 63 nodes");
  }
  let mut state: Vec<Vec<Vec<Option<i64>>>> = vec![vec![vec![None; num_nodes]; num_nodes]; 1 << num_useful_valves];
  // initialize base case
  // no valves open, both workers at node 0, time = 0
  state[0][0][0] = Some(0);
  // dp dp dp dp dp dp
  for t in 0..26 {
    let mut new_state: Vec<Vec<Vec<Option<i64>>>> = vec![vec![vec![None; num_nodes]; num_nodes]; 1 << num_useful_valves];
    println!("t={}", t);
    for bitmask in 0..1 << num_useful_valves {
      for my_pos in 0..num_nodes {
        for el_pos in 0..num_nodes {
          let current = state[bitmask][my_pos][el_pos];
          if current.is_none() {
            continue;
          }
          let current = current.unwrap();
          vec![
            // both you and elephant move
            (0..num_nodes)
              .map(|v| (0..num_nodes)
                .map(|v2| (v, v2))
                .collect::<Vec<_>>()
              )
              .flatten()
              .filter(|v| adj[&(my_pos, v.0)] && adj[&(el_pos, v.1)])
              .map(|v| (bitmask, v.0, v.1))
              .collect::<Vec<_>>(),
            // you move, elephant opens
            (0..num_nodes)
              .filter(|_| flows[el_pos] != 0)
              .filter(|v| adj[&(my_pos, *v)])
              .map(|v| (bitmask | (1 << el_pos), v, el_pos))
              .collect(),
            // you open, elephant moves
            (0..num_nodes)
              .filter(|_| flows[my_pos] != 0)
              .filter(|v| adj[&(el_pos, *v)])
              .map(|v| (bitmask | (1 << my_pos), my_pos, v))
              .collect(),
            // both you and the elephant open
            vec![(bitmask | (1 << my_pos) | (1 << el_pos), my_pos, el_pos)]
              .into_iter()
              .filter(|_| flows[el_pos] != 0 && flows[my_pos] != 0)
              .collect()
          ].iter().flatten().for_each(|v| {
            max_assign(&mut new_state[v.0][v.1][v.2], Some(current + bitmask_to_flow(bitmask as u64, &flows)));
          });
        }
      }
    }
    state = new_state;
  }
  let best_pressure = state.iter()
    .flatten()
    .flatten()
    .reduce(|a, b| max(a, b))
    .unwrap()
    .unwrap();
  println!("{}", best_pressure);
}