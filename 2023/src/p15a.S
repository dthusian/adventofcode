.intel_syntax noprefix

.global p15a
p15a:
  push rbp
  push rbx # accum
  push r8 # current hash
  push r9
  push r10
  push r11
  push r12
  push r13 # file buf mut
  push r14 # file buf
  push r15 # file
  sub rsp,8

  mov r15,rdi
  mov rdi,65536
  call aocc_malloc
  mov r14,rax
  mov r13,rax
  
  mov rdi,r14
  mov rsi,1
  mov rdx,65536
  mov rcx,r15
  call aocc_fread

  xor rbx,rbx
  xor r8,r8
loop0:
  movzx rax,byte ptr[r13]
  cmp rax,'\n'
  je loop0end
  cmp rax,0
  je loop0end
  cmp rax,','
  je br0b
br0a: # normal char
  add r8,rax
  imul r8,r8,17
  and r8,0xff
  jmp br0end
br0b: # ,
  add rbx,r8
  xor r8,r8
br0end:

  inc r13
  jmp loop0
loop0end:
  add rbx,r8

  mov rdi,r14
  call aocc_free

  mov rax,rbx

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