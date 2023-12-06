.intel_syntax noprefix
#include "lib.h.S"

# format: struct { ptrdiff_t offsets[8]; uint64_t data[]; }

# __aocc (FILE*) -> void*
parse_5a:
  push rbp # struct buf
  push rbx # struct buf write ptr
  push r8
  push r9
  push r10
  push r11
  push r12 # array id
  push r13 # FILE* buf mut
  push r14 # FILE* buf
  push r15 # FILE*
  sub rsp,8 # strtol out

  mov r15,rdi
  
  mov rdi,1024*128
  call aocc_malloc
  mov r14,rax

  mov rdi,r14
  mov rsi,1024*128
  mov rdx,1
  mov rcx,r15
  call aocc_fread
  mov r13,r14

  xor r12,r12

  mov rdi,1024*128
  call aocc_malloc
  mov rbp,rax
  lea rbx,[rbp+64]

loop0: # skip first :
  movzx rax,byte ptr[r13]
  inc r13
  cmp rax,':'
  jne loop0
loop0end:

loop1:
  movzx rax,byte ptr[r13]
  # check nul
  cmp rax,0
  je loop1end

  # check colon
  cmp rax,':'
  jne br0end
br0a: 
  mov [rbp+r12*8],rbx
  inc r12
  inc r13
  jmp loop1
br0end:

  # check num
  cmp rax,'0'
  jl br1end
  cmp rax,'9'
  jg br1end
br1a:
  mov rdi,r13
  mov rsi,rsp
  mov rdx,10
  call aocc_strtol
  mov r13,[rsp]
  mov [rbx],rax
  add rbx,8
br1end:

  inc r13
  jmp loop1
loop1end:
  mov [rbp+r12*8],rbx

  mov rdi,r14
  call aocc_free

  mov rax,rbp

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

# (uint64_t, uint64_t* start, uint64_t* stop) -> uint64_t
transform:
  # rax: short term scratch
  # rdi: transforming value
  # rsi: array* mut
  # rdx: stop ptr
  # rcx: scratch
loop2:
  mov rax,rdi
  sub rax,[rsi+8]
  cmp rax,[rsi+16]
  jae br2end
br2a:
  add rax,[rsi]
  mov rdi,rax
  jmp loop2end
br2end:

  add rsi,24
  cmp rsi,rdx
  jb loop2
loop2end:
  mov rax,rdi

  ret

.global p5a
p5a:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11
  push r12 # min
  push r13 # loop4 counter ptr (offsets array)
  push r14 # loop3/5 counter ptr (first array)
  push r15 # parsed struct
  sub rsp,8

  call parse_5a
  mov r15,rax

  lea r14,[r15+64]
loop3:
  
  mov r13,r15
  mov rdi,[r14]
loop4:
  mov rsi,[r13]
  mov rdx,[r13+8]
  call transform
  mov rdi,rax

  add r13,8
  lea rax,[r15+56]
  cmp r13,rax
  jb loop4
loop4end:
  mov [r14],rdi
  
  add r14,8
  cmp r14,[r15]
  jb loop3
loop3end:

  xor r12,r12 # r12 is signed !!
  dec r12
  lea r14,[r15+64]
loop5:
  mov rax,[r14]
  cmp r12,rax
  cmova r12,rax

  add r14,8
  cmp r14,[r15]
  jb loop5
loop5end:

  mov rdi,r15
  call aocc_free

  mov rax,r12

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