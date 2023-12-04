.intel_syntax noprefix

# must be 64-bit regs, arg1 can be immediate

.macro lib_mul arg0,arg1
  sub rsp,8
  push rdx

  mov rax,\arg0
  mov rdx,\arg1
  imul rdx
  mov [rsp+8],rax

  pop rdx
  pop rax
.endm

#if 0
.global p1a
p1a:
  push rbp
  push rbx
  push r12
  push r13
  push r14
  push r15
  sub rsp,8

  add rsp,8
  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx
  pop rbp
  ret
#endif