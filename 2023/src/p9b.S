.intel_syntax noprefix

#define ARR_W 100

# (int64_t* out, int64_t* in, uint64_t count)
build_dx:
  add rsi,8
  dec rdx
loop0:
  mov rax,[rsi]
  sub rax,[rsi-8]
  mov [rdi],rax

  add rdi,8
  add rsi,8
  dec rdx
  jnz loop0
loop0end:
  ret

# (int64_t* in, uint64_t count) -> bool
check_dx:
loop1:
  cmp qword ptr[rdi],0
  je br0end
br0a:
  xor rax,rax
  ret
br0end:
  add rdi,8
  dec rsi
  jnz loop1
loop1end:
  mov rax,1
  ret

# (int64_t* in, uint64_t count) -> void
reverse1999:
  mov rax,rsi
  shr rax,1
  lea rsi,[rdi+rsi*8-8]
loop6:
  mov rcx,[rdi]
  mov rdx,[rsi]
  mov [rdi],rdx
  mov [rsi],rcx
  add rdi,8
  sub rsi,8
  dec rax
  jnz loop6
loop6end:
  ret

# (int64_t* in, uint64_t count) -> int64_t
single_line:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11
  push r12 # sum
  push r13 # i
  push r14 # buf len
  push r15 # buf
  sub rsp,8

  mov r15,rdi
  mov r14,rsi
  xor r13,r13
loop4:
  inc r13

  imul rdi,r13,8*ARR_W
  add rdi,r15
  lea rax,[r13-1]
  imul rsi,rax,8*ARR_W
  add rsi,r15
  mov rdx,r14
  sub rdx,r13
  inc rdx
  call build_dx

  imul rdi,r13,8*ARR_W
  add rdi,r15
  mov rsi,r14
  call check_dx
  
  cmp rax,0
  je loop4
loop4end:

  xor r12,r12
  # rdx: buf mut
  mov rdx,r15
  lea rax,[r14-1]
  shl rax,3
  add rdx,rax
loop5:
  add r12,[rdx]
  add rdx,8*ARR_W-8

  dec r13
  jnz loop5
loop5end:

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

.global p9b
p9b:
  push rbp # file
  push rbx
  push r8
  push r9
  push r10 # sum
  push r11 # number of el
  push r12 # cline mut
  push r13 # cline
  push r14 # obj mut
  push r15 # obj
  sub rsp,8

  mov rbp,rdi

  mov rdi,8*ARR_W*ARR_W
  call aocc_malloc
  mov r15,rax

  mov rdi,1024
  call aocc_malloc
  mov r13,rax

  xor r10,r10

loop2:
  # reset buf
  mov rdi,r15
  mov rsi,0
  mov rdx,8*ARR_W*ARR_W
  call aocc_memset

  mov rdi,r13
  mov rsi,1024
  mov rdx,rbp
  call aocc_fgets
  cmp rax,0
  je loop2end

  # reset ptrs
  mov r14,r15
  mov r12,r13
  xor r11,r11
loop3:
  movzx rax,byte ptr[r12]
  cmp rax,0
  je loop3end
  cmp rax,'-'
  je br1a
  cmp rax,'0'
  jl br1end
  cmp rax,'9'
  jg br1end
br1a:
  mov rdi,r12
  mov rsi,rsp
  mov rdx,10
  call aocc_strtoll
  mov r12,[rsp]
  dec r12
  mov [r14],rax
  add r14,8
  inc r11
br1end:
  inc r12
  jmp loop3
loop3end:

  mov rdi,r15
  mov rsi,r11
  call reverse1999

  mov rdi,r15
  mov rsi,r11
  call single_line
  add r10,rax

  jmp loop2
loop2end:

  mov rax,r10

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