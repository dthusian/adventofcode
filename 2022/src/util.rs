use std::fs::File;
use std::io::Read;
use std::sync::mpsc::Receiver;
use bstr::BString;
use glfw::{Glfw, OpenGlProfileHint, SwapInterval, Window, WindowEvent, WindowHint};
use crate::gl;

pub fn input() -> BString {
  let mut input = BString::default();
  File::open("input.txt").unwrap().read_to_end(&mut input).unwrap();
  return input;
}

pub fn glfw() -> (Glfw, Window, Receiver<(f64, WindowEvent)>) {
  let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
  glfw.window_hint(WindowHint::ContextVersion(4, 5));
  glfw.window_hint(WindowHint::OpenGlProfile(OpenGlProfileHint::Core));
  let (window, events) = glfw.create_window(400, 400, "tmp3", glfw::WindowMode::Windowed).unwrap();
  glfw.make_context_current(Some(&window));
  gl::load(|e| glfw.get_proc_address_raw(e));
  glfw.set_swap_interval(SwapInterval::Sync(1));
  (glfw, window, events)
}