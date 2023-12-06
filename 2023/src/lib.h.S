.intel_syntax noprefix

# aocc calling conv
# rdi: arg1 (clobbered)
# rsi: arg2 (clobbered)
# rdx: arg3 (clobbered)
# rcx: arg4 (clobbered)
# rax: return value (clobbered)
# rbx: saved
# rbp: saved
# r8-15: saved

#if 0
.global fn
fn:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11
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
  pop r11
  pop r10
  pop r9
  pop r8
  pop rbx
  pop rbp
  ret
#endif