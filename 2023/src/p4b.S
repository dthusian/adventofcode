.intel_syntax noprefix

.global p4b
p4b:
  push rbp # curr line points
  push rbx # bitvec
  push r12 # loop0 cnt
  push r13 # number of Nth card
  push r14 # curr line mut
  push r15 # curr line
  sub rsp,24
  # - final sum save space
  # - FILE*
  # - strtol out

  mov [rsp+8],rdi

  mov rdi,1024
  call malloc
  mov r15,rax

  mov rdi,2000
  call malloc
  mov r13,rax
  mov rdi,rax
  mov rsi,0
  mov rdx,2000
  call memset

  mov rdi,128
  call malloc
  mov rbx,rax

  xor r12,r12
  inc r12
loop0:
  xor rbp,rbp
  mov rdi,rbx
  mov rsi,0
  mov rdx,128
  call memset

  mov rdi,r15
  mov rsi,1024
  mov rdx,[rsp+8]
  call fgets
  cmp rax,0
  je loop0end

  mov r14,r15
loop1:
  movzx rax,byte ptr[r14]
  inc r14
  cmp rax,':'
  jne loop1
loop1end:

  # scan for scratch-card numbers
loop2:
  movzx rax,byte ptr[r14]
  cmp rax,'|'
  je loop2end
  cmp rax,'0'
  jl loop2cnt
  cmp rax,'9'
  jg loop2cnt
  # parse num
  mov rdi,r14
  mov rsi,rsp
  mov rdx,10
  call strtol
  mov r14,[rsp]
  # set bits
  mov byte ptr[rbx+rax],1

loop2cnt:
  inc r14
  jmp loop2
loop2end:

loop3:
  movzx rax,byte ptr[r14]
  cmp rax,0
  je loop3end
  cmp rax,'0'
  jl loop3cnt
  cmp rax,'9'
  jg loop3cnt
  # parse num
  mov rdi,r14
  mov rsi,rsp
  mov rdx,10
  call strtol
  mov r14,[rsp]
  # test bits
  movzx rax,byte ptr[rbx+rax]
  add rbp,rax
loop3cnt:
  inc r14
  jmp loop3
loop3end:

  # only if rbp>0
  cmp rbp,0
  je loop4end
  # add it to the array
  mov rcx,rbp # loop counter
  mov edx,dword ptr[r13+r12*4] # how many of this card (+1 original)
  inc rdx
loop4:
  lea rax,[r12+rcx]
  shl rax,2
  add dword ptr[r13+rax],edx
  dec rcx
  jnz loop4
loop4end:

  inc r12
  jmp loop0
loop0end:

  # just sum [r13] into rax
  xor rax,rax
  mov rcx,r12
loop5:
  add eax,dword ptr[r13+rcx*4]
  inc eax
  dec rcx
  jnz loop5
loop5end:
  dec eax
  mov [rsp+16],rax

  mov rdi,r15
  call free
  mov rdi,rbx
  call free
  mov rdi,r13
  call free

  mov rax,[rsp+16]

  add rsp,24
  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx
  pop rbp
  ret