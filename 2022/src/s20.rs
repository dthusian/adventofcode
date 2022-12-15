use std::collections::VecDeque;
use std::fmt::Debug;
use std::str::FromStr;
use bstr::ByteSlice;
use itermore::IterSorted;
use crate::util::input;

#[derive(Debug, Clone)]
enum Op {
  Add(i64),
  Mul(i64),
  Sq
}

impl Op {
  pub fn exec(&self, x: i64) -> i64 {
    match self {
      Op::Add(y) => x + *y,
      Op::Mul(y) => x * *y,
      Op::Sq => x * x
    }
  }
}

#[derive(Debug, Clone)]
struct Monkey {
  pub buf: VecDeque<i64>,
  pub op: Op,
  pub test: i64,
  pub throw_true: usize,
  pub throw_false: usize,
  pub inspect_total: i64
}

impl Monkey {
  pub fn throw(&self, check: i64) -> usize {
    if check % self.test == 0 {
      self.throw_true
    } else {
      self.throw_false
    }
  }
}

#[derive(Debug, Clone)]
struct State(pub Vec<Monkey>, pub i64);

impl State {
  pub fn business(&self) -> i64 {
    let v = self.0.iter().map(|v| v.inspect_total).sorted().rev().take(2).collect::<Vec<_>>();
    v[0] * v[1]
  }
}

fn parse() -> State {
  fn parse_last<T: FromStr>(s: &[u8]) -> T
    where <T as FromStr>::Err: Debug
  {
    s.split_str(" ").collect::<Vec<_>>().last().unwrap().to_str_lossy().parse::<T>().unwrap()
  }

  let mut monkeys = vec![];
  let mut lcm = 1;
  for line in input().split_str("\n") {
    let spl = line.split_str(":").collect::<Vec<_>>();
    let prefix = spl[0].to_str_lossy();
    if prefix.starts_with("Monkey") {
      monkeys.push(Monkey {
        buf: Default::default(),
        op: Op::Sq,
        test: 0,
        throw_true: 0,
        throw_false: 0,
        inspect_total: 0
      });
    } else if prefix.starts_with("  Starting items") {
      let items = spl[1][1..].split_str(", ").map(|v| v.to_str_lossy().parse().unwrap()).collect();
      monkeys.last_mut().unwrap().buf = items;
    } else if prefix.starts_with("  Operation") {
      let spl2 = spl[1].split_str(" ").collect::<Vec<_>>();
      let op = if spl2[spl2.len() - 2] == b"+" {
        Op::Add(spl2.last().unwrap().to_str_lossy().parse().unwrap())
      } else if spl2[spl2.len() - 2] == b"*" {
        let operand = spl2.last().unwrap().to_str_lossy();
        if operand == "old" {
          Op::Sq
        } else {
          Op::Mul(operand.parse().unwrap())
        }
      } else {
        panic!("Invalid op")
      };
      monkeys.last_mut().unwrap().op = op;
    } else if prefix.starts_with("  Test") {
      let div = parse_last(spl[1]);
      lcm *= div;
      monkeys.last_mut().unwrap().test = div;
    } else if prefix.starts_with("    If true") {
      monkeys.last_mut().unwrap().throw_true = parse_last(spl[1]);
    } else if prefix.starts_with("    If false") {
      monkeys.last_mut().unwrap().throw_false = parse_last(spl[1]);
    }
  }
  State(monkeys, lcm)
}

fn round(state: &mut State) {
  for i in 0..state.0.len() {
    while !state.0[i].buf.is_empty() {
      let monkey = &mut state.0[i];
      let thing = monkey.buf.pop_front().unwrap();
      let thing = monkey.op.exec(thing) / 3;
      let throw_to = monkey.throw(thing);
      monkey.inspect_total += 1;
      state.0[throw_to].buf.push_back(thing);
    }
  }
}

pub fn main() {
  let mut state = parse();
  for _ in 0..20 {
    round(&mut state);
  }
  println!("{}", state.business());
}