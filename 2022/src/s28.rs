use std::ffi::c_void;
use bstr::{BString, ByteSlice};
use glfw::Context;
use crate::gl;
use crate::util::{gl_shader, gl_tex1d, gl_tex2d, glfw, input};

fn parse(input: BString) -> Vec<(i32, i32, i32)> {
  input.split_str("\n")
    .map(|v| v.split_str(" ").collect::<Vec<_>>())
    .map(|v| (
      v[2].strip_prefix(b"x=").unwrap().strip_suffix(b",").unwrap(),
      v[3].strip_prefix(b"y=").unwrap().strip_suffix(b":").unwrap(),
      v[8].strip_prefix(b"x=").unwrap().strip_suffix(b",").unwrap(),
      v[9].strip_prefix(b"y=").unwrap()
    ))
    .map(|v| (
      v.0.to_str_lossy().parse::<i32>().unwrap(),
      v.1.to_str_lossy().parse::<i32>().unwrap(),
      v.2.to_str_lossy().parse::<i32>().unwrap(),
      v.3.to_str_lossy().parse::<i32>().unwrap()
    ))
    .map(|v| (v.0, v.1, (v.0 - v.2).abs() + (v.1 - v.3).abs()))
    .collect()
}

pub fn main() {
  unsafe {
    let input = parse(input()).into_iter().map(|v| vec![v.0, v.1, v.2, 0i32]).flatten().collect::<Vec<_>>();
    let mut outputbuf = vec![0i32; 1];
    println!("{:?}", input);
    let mut glfw = glfw();
    let mut max_cs_size = vec![0i32; 3];
    gl::GetIntegeri_v(gl::MAX_COMPUTE_WORK_GROUP_COUNT, 0, max_cs_size.as_mut_ptr());
    gl::GetIntegeri_v(gl::MAX_COMPUTE_WORK_GROUP_COUNT, 1, max_cs_size.as_mut_ptr().offset(1));
    gl::GetIntegeri_v(gl::MAX_COMPUTE_WORK_GROUP_COUNT, 2, max_cs_size.as_mut_ptr().offset(2));
    println!("max cs size: {:?}", max_cs_size);
    let sensors = gl_tex1d(gl::RGBA32I, input.len() / 4, gl::RGBA_INTEGER, gl::INT, input.as_ptr() as *const c_void);
    let output = gl_tex1d(gl::R32I, 1, gl::RED_INTEGER, gl::INT, outputbuf.as_ptr() as *const c_void);
    gl::BindImageTexture(0, sensors, 0, gl::FALSE, 0, gl::READ_ONLY, gl::RGBA32I);
    gl::BindImageTexture(1, output, 0, gl::FALSE, 0, gl::READ_WRITE, gl::R32I);
    let shader = gl_shader("
#version 450 core

layout(local_size_x = 1) in;
layout(binding = 0, rgba32i) uniform iimage1D sensors;
layout(binding = 1, r32i) uniform iimage1D outbuf;

bool can_have_beacon(ivec2 pos) {
  bool can = true;
  for(int i = 0; i < imageSize(sensors).x; i++) {
    ivec3 current = imageLoad(sensors, i).xyz;
    if(abs(pos.x - current.x) + abs(pos.y - current.y) <= current.z) {
      can = false;
    }
  }
  return can;
}

void main() {
  ivec2 offset = ivec2(-250000, 2000000);
  int one_dim_id = int(gl_GlobalInvocationID.x) + int(gl_GlobalInvocationID.y) * 50000;
  if(!can_have_beacon(ivec2(one_dim_id) + offset)) {
    imageAtomicAdd(outbuf, 0, 1);
  }
}
".into());
    gl::UseProgram(shader);
    gl::DispatchCompute(50000, 50, 1);
    gl::MemoryBarrier(gl::TEXTURE_UPDATE_BARRIER_BIT);
    gl::GetTextureImage(output, 0, gl::RED, gl::INT, 4, outputbuf.as_mut_ptr() as *mut c_void);
    println!("{}", outputbuf[0]);
    glfw.1.swap_buffers();
  }
}