use std::borrow::Borrow;
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

fn sum_large_dir_sizes(dent: &Dentry) -> usize {
  match dent {
    Dentry::Dir(dir, total) => {
      let mut sum = 0;
      if *total <= 100000 {
        sum += total;
      }
      sum += dir.values().map(|v| sum_large_dir_sizes(v)).sum::<usize>();
      sum
    },
    Dentry::File(_) => 0
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
  println!("{}", sum_large_dir_sizes(&root));
}