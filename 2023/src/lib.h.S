.intel_syntax noprefix

# must be 64-bit regs, arg1 can be immediate

.macro lib_mul arg0,arg1
  push rax
  push rdx

  mov rax,\arg0
  mov rdx,\arg1
  imul rdx
  mov [rsp-8],rax

  pop rdx
  pop rax
  mov \arg0,[rsp-24]
.endm

.macro lib_div arg0,arg1
  push rax
  push rdx
  push rcx

  xor edx,edx
  mov rax,\arg0
  mov rcx,\arg1
  idiv rcx
  mov [rsp-8],rax

  pop rcx
  pop rdx
  pop rax
  mov \arg0,[rsp-32]
.endm

.macro lib_mod arg0,arg1
  push rax
  push rdx
  push rcx

  xor edx,edx
  mov rax,\arg0
  mov rcx,\arg1
  idiv rcx
  mov [rsp-8],rdx

  pop rcx
  pop rdx
  pop rax
  mov \arg0,[rsp-32]
.endm