use std::collections::HashMap;
use bstr::ByteSlice;
use crate::gl::NONE;
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

  pub fn args(&self) -> Option<(&str, &str)> {
    match self {
      Expr::Number(_) => None,
      Expr::Add(a, b) => Some((a, b)),
      Expr::Sub(a, b) => Some((a, b)),
      Expr::Mul(a, b) => Some((a, b)),
      Expr::Div(a, b) => Some((a, b))
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

fn simplify(tree: &mut HashMap<String, Expr>, key: &str) -> Option<i64> {
  if key == "humn" {
    return None;
  }
  let el = tree[key].clone();
  let args = match el {
    Expr::Number(x) => return Some(x),
    _ => el.args().unwrap()
  };
  let ls = simplify(tree, args.0);
  let rs = simplify(tree, args.1);
  if ls.is_some() && rs.is_some() {
    let ls = ls.unwrap();
    let rs = rs.unwrap();
    let v = match el {
      Expr::Add(_, _) => ls + rs,
      Expr::Sub(_, _) => ls - rs,
      Expr::Mul(_, _) => ls * rs,
      Expr::Div(_, _) => ls / rs,
      _ => unreachable!()
    };
    *tree.get_mut(key).unwrap() = Expr::Number(v);
    Some(v)
  } else {
    None
  }
}

fn prune(tree: &HashMap<String, Expr>, key: &str, dest: &mut HashMap<String, Expr>) {
  let node = tree[key].clone();
  if let Some((a, b)) = node.args() {
    prune(tree, a, dest);
    prune(tree, b, dest);
  }
  dest.insert(key.into(), node);
}

fn stringify(tree: &HashMap<String, Expr>, key: &str) -> String {
  if key == "humn" {
    return "x".into()
  }
  if key == "root" {
    let (a, b) = tree[key].args().unwrap();
    return format!("{} = {}", stringify(tree, a), stringify(tree, b));
  }
  match &tree[key] {
    Expr::Number(v) => v.to_string(),
    Expr::Add(a, b) => format!("({} + {})", stringify(tree, a), stringify(tree, b)),
    Expr::Sub(a, b) => format!("({} - {})", stringify(tree, a), stringify(tree, b)),
    Expr::Mul(a, b) => format!("({} * {})", stringify(tree, a), stringify(tree, b)),
    Expr::Div(a, b) => format!("({} / {})", stringify(tree, a), stringify(tree, b))
  }
}

fn equate(tree: &HashMap<String, Expr>, key: &str, equals: i64) -> i64 {
  if key == "humn" {
    return equals;
  }
  fn get_num(tree: &HashMap<String, Expr>, key: &str) -> Option<i64> {
    if key == "humn" {
      return None
    }
    match &tree[key] {
      Expr::Number(v) => Some(*v),
      _ => None
    }
  }
  let (a, b) = tree[key].args().unwrap();
  let na = get_num(tree, a);
  let nb = get_num(tree, b);
  if na.is_none() == nb.is_none() {
    panic!("what");
  }
  let v = na.or(nb).unwrap();
  let unknown = if na.is_none() { a } else { b };
  match &tree[key] {
    Expr::Add(_, _) => equate(tree, unknown, equals - v),
    Expr::Sub(_, _) => equate(tree, unknown, if na.is_none() { /* e = u - v */ equals + v } else { /* e = v - u */ -(equals - v) }),
    Expr::Mul(_, _) => equate(tree, unknown, equals / v),
    Expr::Div(_, _) => equate(tree, unknown, if na.is_none() { /* e = u / v */ equals * v } else { panic!("i hate recip") }),
    _ => panic!("bad")
  }
}

fn extract_root_expression(tree2: &HashMap<String, Expr>) -> (&str, i64) {
  let root = &tree2["root"];
  let args = root.args().unwrap();
  let equals;
  let key;
  let maybe_a = match &tree2[args.0] {
    Expr::Number(v) => Some(v),
    Expr::Add(_, _) => None,
    Expr::Sub(_, _) => None,
    Expr::Mul(_, _) => None,
    Expr::Div(_, _) => None
  };
  if maybe_a == None {
    key = args.0;
    equals = match &tree2[args.1] {
      Expr::Number(v) => *v,
      _ => panic!("bad")
    }
  } else {
    key = args.1;
    equals = match &tree2[args.0] {
      Expr::Number(v) => *v,
      _ => panic!("bad")
    }
  }
  (key, equals)
}

pub fn main() {
  let mut tree = input()
    .split_str("\n")
    .map(|v| {
      let part = v.split_once_str(": ").unwrap();
      (part.0.to_str_lossy().to_string(), Expr::parse(&part.1.to_str_lossy()))
    })
    .collect::<HashMap<_, _>>();
  simplify(&mut tree, "root");
  let mut tree2 = HashMap::new();
  prune(&tree, "root", &mut tree2);
  println!("{}", stringify(&tree2, "root"));
  let root_eq = extract_root_expression(&tree2);
  println!("{}", equate(&tree2, root_eq.0, root_eq.1 as i64))
}