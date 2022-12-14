use std::borrow::Borrow;
use std::cmp::min;
use std::collections::HashMap;
use std::str::FromStr;
use bstr::{BStr, BString, ByteSlice};
use crate::util::input;

#[derive(Debug)]
enum Dentry {
  Dir(HashMap<BString, Dentry>, usize),
  File(usize)
}

impl Default for Dentry {
  fn default() -> Self {
    Dentry::Dir(HashMap::default(), usize::MAX)
  }
}

impl Dentry {
  pub fn go_to(&mut self, path: &[BString]) -> &mut Dentry {
    if path.len() == 0 {
      return self;
    }
    match self {
      Dentry::Dir(dir, _) => dir.get_mut::<BString>(&path[0]).unwrap().go_to(&path[1..]),
      Dentry::File(_) => panic!("traverse into file")
    }
  }

  pub fn mkdir(&mut self, name: &BStr) {
    match self {
      Dentry::Dir(dir, _) => dir.insert(name.into(), Dentry::default()),
      Dentry::File(_) => panic!("mkdir on file")
    };
  }

  pub fn touch(&mut self, name: &BStr, size: usize) {
    match self {
      Dentry::Dir(dir, _) => dir.insert(name.into(), Dentry::File(size)),
      Dentry::File(_) => panic!("touch on file")
    };
  }
}

fn populate_dir_sizes(dent: &mut Dentry) -> usize {
  match dent {
    Dentry::Dir(dir, total) => {
      *total = dir.values_mut().map(|v| populate_dir_sizes(v)).sum();
      *total
    },
    Dentry::File(size) => *size
  }
}

fn better(a: usize, b: usize, needed: usize) -> usize {
  let mut a = a;
  let mut b = b;
  if a < needed {
    a = usize::MAX;
  }
  if b < needed {
    b = usize::MAX;
  }
  min(a, b)
}

fn find_optimal_dir(dent: &Dentry, needed: usize) -> usize {
  match dent {
    Dentry::Dir(dir, total) =>
      better(*total, dir.values()
        .map(|v| find_optimal_dir(v, needed))
        .reduce(|a, b| better(a, b, needed))
        .or(Some(0))
        .unwrap(), needed
      ),
    Dentry::File(_) => usize::MAX
  }
}

pub fn main() {
  let input = input();
  let mut wd = vec![];
  let mut root = Dentry::Dir(Default::default(), usize::MAX);
  for line in input.split_str("\n") {
    if line[0] == b'$' {
      let spl = line.split_str(" ").collect::<Vec<_>>();
      if spl[1] == b"cd" {
        if spl[2] == b".." {
          wd.pop();
        } else if spl[2] == b"/" {
          wd = vec![];
        } else {
          wd.push(BString::from(spl[2]));
          if let Dentry::File(_) = root.go_to(&wd) {
            panic!("cd into file");
          }
        }
      } else if spl[1] == b"ls" {
        //nothing
      } else {
        panic!("Unknown command: {}", spl[1].to_str_lossy());
      }
    } else {
      let spl = line.split_str(" ").collect::<Vec<_>>();
      let dent = root.go_to(&wd);
      if spl[0] == b"dir" {
        dent.mkdir(spl[1].into());
      } else {
        dent.touch(spl[1].into(), usize::from_str(&String::from_utf8_lossy(spl[0])).expect("int parse error"));
      }
    }
  }
  populate_dir_sizes(&mut root);
  let needed_size = match root {
    Dentry::Dir(_, total) => 30_000_000 - (70_000_000 - total),
    Dentry::File(_) => panic!("bruh")
  };
  println!("{}", find_optimal_dir(&root, needed_size));
}