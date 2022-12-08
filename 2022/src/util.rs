use std::ffi::c_void;
use std::fs::File;
use std::io::Read;
use std::mem;
use std::sync::mpsc::Receiver;
use bstr::BString;
use glfw::{Glfw, OpenGlProfileHint, SwapInterval, Window, WindowEvent, WindowHint};
use crate::gl;
use crate::gl::{GLenum, GLint, GLsizei, GLuint};

pub fn input() -> BString {
  let mut input = BString::default();
  File::open("input.txt").unwrap().read_to_end(&mut input).unwrap();
  input
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

pub unsafe fn gl_tex(fmt: GLenum, w: usize, h: usize, data_fmt: GLenum, data_type: GLenum, data: *const c_void) -> GLuint {
  let mut tex = 0;
  gl::GenTextures(1, &mut tex);
  gl::BindTexture(gl::TEXTURE_2D, tex);
  gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::NEAREST as GLint);
  gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::NEAREST as GLint);
  gl::TexImage2D(gl::TEXTURE_2D, 0, fmt as GLint, w as GLsizei, h as GLsizei, 0, data_fmt, data_type, data);
  tex
}

pub unsafe fn gl_shader(glsl: String) -> GLuint {
  let shader = gl::CreateShader(gl::COMPUTE_SHADER);
  let glsl_ptr = glsl.as_ptr();
  let glsl_len = glsl.len() as GLint;
  gl::ShaderSource(shader, 1, mem::transmute::<&*const u8, *const *const i8>(&glsl_ptr), &glsl_len);
  gl::CompileShader(shader);
  let mut stat: GLint = 0xff;
  gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut stat);
  if stat == gl::FALSE as GLint {
    let mut buf = vec![0u8; 1024];
    let mut len = 0;
    gl::GetShaderInfoLog(shader, 1024, &mut len, mem::transmute::<*mut u8, *mut i8>(buf.as_mut_ptr()));
    gl::DeleteShader(shader);
    panic!("Shader compilation failed: {}", String::from_utf8_lossy(&buf[0..len as usize]));
  }
  let program = gl::CreateProgram();
  gl::AttachShader(program, shader);
  gl::LinkProgram(program);
  gl::DeleteShader(shader);
  gl::GetProgramiv(program, gl::LINK_STATUS, &mut stat);
  if stat == gl::FALSE as GLint {
    gl::DeleteProgram(program);
    panic!("Shader link failed");
  }
  program
}