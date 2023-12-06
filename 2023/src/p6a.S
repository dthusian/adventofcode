.intel_syntax noprefix

# (FILE*) -> int[8]
parse_6a:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11 # buf mut
  push r12 # buf
  push r13 # filebuf mut
  push r14 # filebuf
  push r15 # FILE*
  sub rsp,8

  mov r15,rdi

  mov rdi,65536
  call aocc_malloc
  mov r14,rax
  mov r13,rax

  mov rdi,64
  call aocc_malloc
  mov r11,rax
  mov r12,rax

  mov rdi,r13
  mov rsi,65536
  mov rdx,1
  mov rcx,r15
  call aocc_fread

loop0:
  movzx rax,byte ptr[r13]

  cmp rax,0
  je loop0end

  cmp rax,'0'
  jl br0end
  cmp rax,'9'
  jg br0end
br0a:
  mov rdi,r13
  mov rsi,rsp
  mov rdx,10
  call aocc_strtol
  mov r13,[rsp]
  mov [r11],rax
  add r11,8
br0end:

  inc r13
  jmp loop0
loop0end:

  mov rdi,r14
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

.global p6a
p6a:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11
  push r12 # outer accumulator
  push r13 # outer loop counter
  push r14
  push r15 # parsed
  sub rsp,8

  call parse_6a
  mov r15,rax

  mov r12,1
  mov r13,4

loop1:
  mov r8,[r15+r13*8-8]  # t
  mov r9,[r15+r13*8+24] # d

  xor rax,rax # counter
  mov rcx,r8 # loop counter
loop2:
  mov rdi,r8
  sub rdi,rcx # total_time - hold_time
  imul rdi,rcx

  xor rdx,rdx
  mov rsi,1
  cmp rdi,r9
  cmovg rdx,rsi
  add rax,rdx

  dec rcx
  jnz loop2
loop2end:

  imul r12,rax

  dec r13
  jnz loop1  
loop1end:

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