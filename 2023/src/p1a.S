.intel_syntax noprefix

.global p1a
p1a:
  # rdx - scratch
  push rbp # first
  push rbx # last
  push r12 # sum
  push r13 # curr line
  push r14 # FILE* input
  push r15 # pos in curr line

  mov r14,rdi

  mov rdi,4096
  call malloc
  mov r13,rax

  xor rax,rax
  xor r12,r12
  xor rbp,rbp
  xor rbx,rbx
loop0:

  xor r15,r15
  mov rdi,r13
  mov rsi,4096
  mov rdx,r14
  call fgets
  cmp rax,0
  jz loop0br

loop1:
  xor rax,rax
  mov al,byte ptr[r13+r15]
  movzx rax,al

  cmp rax,'0'
  jl br2
  cmp rax,'9'
  jg br2
  
  cmp rbp,0
  cmove rbp,rax
  mov rbx,rax

br2: # other

  inc r15
  cmp byte ptr[r13+r15],0
  jne loop1

  # process rbp/rbx and add to cnt

  sub rbp,'0'
  sub rbx,'0'

  push rax
  mov rax,10
  mul rbp
  add rbx,rax
  pop rax
  add r12,rbx
  
  xor rbp,rbp
  xor rbx,rbx

  jmp loop0
loop0br:

  mov rax,r12

  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx
  pop rbp
  ret