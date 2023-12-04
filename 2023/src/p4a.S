.intel_syntax noprefix

.global p4a
p4a:
  push rbp # curr line points
  push rbx # bitvec
  push r12 # FILE*
  push r13 # curr line mut
  push r14 # sum points
  push r15 # curr line
  sub rsp,8 # strtol out

  mov r12,rdi

  mov rdi,1024
  call malloc
  mov r15,rax

  xor r14,r14

  mov rdi,128
  call malloc
  mov rbx,rax

loop0:
  xor rbp,rbp
  mov rdi,rbx
  mov rsi,0
  mov rdx,128
  call memset

  mov rdi,r15
  mov rsi,1024
  mov rdx,r12
  call fgets
  cmp rax,0
  je loop0end

  mov r13,r15
loop1:
  movzx rax,byte ptr[r13]
  inc r13
  cmp rax,':'
  jne loop1
loop1end:

  # scan for scratch-card numbers
loop2:
  movzx rax,byte ptr[r13]
  cmp rax,'|'
  je loop2end
  cmp rax,'0'
  jl loop2cnt
  cmp rax,'9'
  jg loop2cnt
  # parse num
  mov rdi,r13
  mov rsi,rsp
  mov rdx,10
  call strtol
  mov r13,[rsp]
  # set bits
  mov byte ptr[rbx+rax],1

loop2cnt:
  inc r13
  jmp loop2
loop2end:

loop3:
  movzx rax,byte ptr[r13]
  cmp rax,0
  je loop3end
  cmp rax,'0'
  jl loop3cnt
  cmp rax,'9'
  jg loop3cnt
  # parse num
  mov rdi,r13
  mov rsi,rsp
  mov rdx,10
  call strtol
  mov r13,[rsp]
  # test bits
  movzx rax,byte ptr[rbx+rax]
  add rbp,rax
loop3cnt:
  inc r13
  jmp loop3
loop3end:

  mov rcx,rbp
  mov rbp,1
  shl rbp,cl
  shr rbp,1
  add r14,rbp

  jmp loop0
loop0end:

  mov rdi,r15
  call free

  mov rax,r14

  add rsp,8
  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx
  pop rbp
  ret