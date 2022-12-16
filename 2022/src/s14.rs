use std::ffi::c_void;
use std::{mem, ptr};
use std::ptr::null;
use bstr::{BString, ByteSlice, ByteVec};
use glfw::Context;
use crate::gl;
use crate::gl::{GLsizei, GLuint};
use crate::util::{gl_shader, gl_tex2d, glfw, input};

pub fn main() {
  unsafe {
    let raw_input = input();
    let input = raw_input.iter().filter(|v| v.is_ascii_digit()).map(|v| *v as i32).collect::<Vec<_>>();
    let mut glfw = glfw();
    let w = raw_input.find_byte(b'\n').unwrap();
    let h = raw_input.split_str(b"\n").count();
    let tex_input = gl_tex2d(gl::R32I, w, h, gl::RED_INTEGER, gl::INT, input.as_ptr() as *const c_void);
    let tex_output = gl_tex2d(gl::R32I, w, h, gl::RED_INTEGER, gl::INT, null());
    let program = gl_shader("
#version 450 core

layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0, r32i) uniform iimage2D img_in;
layout(binding = 1, r32i) uniform iimage2D img_out;

void main() {
  ivec2 sz = imageSize(img_out).xy;
  ivec2 pos = ivec2(gl_GlobalInvocationID);
  int my_height = imageLoad(img_in, pos).x - 48;

  bool visible = false;
  bool this_visible = true;

  this_visible = true;
  for(int i = pos.x + 1; i < sz.x; i++) {
    int height = imageLoad(img_in, ivec2(i, pos.y)).x - 48;
    if(height >= my_height) {
      this_visible = false;
    }
  }
  if(this_visible) { visible = true; }

  this_visible = true;
  for(int i = pos.x - 1; i >= 0; i--) {
    int height = imageLoad(img_in, ivec2(i, pos.y)).x - 48;
    if(height >= my_height) {
      this_visible = false;
    }
  }
  if(this_visible) { visible = true; }

  this_visible = true;
  for(int i = pos.y + 1; i < sz.y; i++) {
    int height = imageLoad(img_in, ivec2(pos.x, i)).x - 48;
    if(height >= my_height) {
      this_visible = false;
    }
  }
  if(this_visible) { visible = true; }

  this_visible = true;
  for(int i = pos.y - 1; i >= 0; i--) {
    int height = imageLoad(img_in, ivec2(pos.x, i)).x - 48;
    if(height >= my_height) {
      this_visible = false;
    }
  }
  if(this_visible) { visible = true; }

  imageStore(img_out, pos, ivec4(visible ? 1 : 0));
}
".into());
    gl::BindImageTexture(0, tex_input, 0, gl::FALSE, 0, gl::READ_ONLY, gl::R32I);
    gl::BindImageTexture(1, tex_output, 0, gl::FALSE, 0, gl::WRITE_ONLY, gl::R32I);
    gl::UseProgram(program);
    gl::DispatchCompute(w as GLuint, h as GLuint, 1);
    gl::MemoryBarrier(gl::TEXTURE_UPDATE_BARRIER_BIT);
    let mut output = vec![0i32; w * h];
    gl::BindTexture(gl::TEXTURE_2D, tex_output);
    gl::GetTexImage(gl::TEXTURE_2D, 0, gl::RED_INTEGER, gl::INT, output.as_mut_ptr() as *mut c_void);
    let mut count = 0;
    for b in output {
      if b == 1 { count += 1; }
    }
    glfw.1.swap_buffers();
    println!("{}", count);
  }
}