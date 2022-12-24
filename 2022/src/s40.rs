use std::collections::HashMap;
use bstr::ByteSlice;
use crate::util::input;

#[derive(Clone, Debug)]
enum Expr {
  Number(i64),
  Add(String, String),
  Sub(String, String),
  Mul(String, String),
  Div(String, String)
}

impl Expr {
  pub fn parse(s: &str) -> Self {
    if s.as_bytes()[0].is_ascii_digit() {
      return Expr::Number(s.parse::<i64>().unwrap());
    }
    let spl = s.split(" ").collect::<Vec<_>>();
    match spl[1] {
      "+" => Expr::Add(spl[0].into(), spl[2].into()),
      "-" => Expr::Sub(spl[0].into(), spl[2].into()),
      "*" => Expr::Mul(spl[0].into(), spl[2].into()),
      "/" => Expr::Div(spl[0].into(), spl[2].into()),
      &_ => panic!("invalid expr")
    }
  }
}

fn eval(tree: &HashMap<String, Expr>, key: &str) -> i64 {
  let expr = &tree[key];
  match expr {
    Expr::Number(x) => *x,
    Expr::Add(a, b) => eval(tree, a) + eval(tree, b),
    Expr::Sub(a, b) => eval(tree, a) - eval(tree, b),
    Expr::Mul(a, b) => eval(tree, a) * eval(tree, b),
    Expr::Div(a, b) => eval(tree, a) / eval(tree, b)
  }
}

pub fn main() {
  let input = input()
    .split_str("\n")
    .map(|v| {
      let part = v.split_once_str(": ").unwrap();
      (part.0.to_str_lossy().to_string(), Expr::parse(&part.1.to_str_lossy()))
    })
    .collect::<HashMap<_, _>>();
  println!("{}", eval(&input, "root"));
}