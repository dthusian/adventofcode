use bstr::ByteSlice;
use crate::util::input;

pub fn main() {
  let input = input();
  let instrs = input.split_str("\n")
    .map(|v| v.split_str(" ").collect::<Vec<_>>())
    .map(|v| (v[0].to_str_lossy().into_owned(), v.get(1).map(|v| v.to_str_lossy().parse::<i64>().unwrap())))
    .collect::<Vec<_>>();
  let check_times = vec![20, 60, 100, 140, 180, 220];
  let mut cycle_count = 1;
  let mut pc = 0;
  let mut cycles_left = 0;
  let mut future_addx = 0;
  let mut x = 1;
  let mut strength = 0;
  loop {
    if cycles_left <= 0 {
      x += future_addx;
      future_addx = 0;
    }
    if check_times.contains(&cycle_count) {
      strength += cycle_count * x;
    }
    if cycles_left <= 0 {
      if instrs.len() <= pc {
        break;
      }
      let instr = instrs[pc].clone();
      if instr.0 == "addx" {
        cycles_left = 1;
        future_addx = instr.1.unwrap();
      } else {
        cycles_left = 0;
      }
      pc += 1;
    } else {
      cycles_left -= 1;
    }
    cycle_count += 1;
  }
  println!("{}", strength);
}