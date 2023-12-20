.intel_syntax noprefix

# struct obj { uint32_t bits[32]; uint32_t bits_trans[32]; }

parse_13b:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11
  push r12
  push r13 # obj mut
  push r14 # obj
  push r15 # file
  sub rsp,72

  mov r15,rdi

  mov rdi,r15
  call feof
  cmp rax,0
  je br0end
br0a:
  xor rax,rax
  jmp ret0
br0end:

  mov rdi,256
  call aocc_malloc
  mov r14,rax
  mov r13,rax
  mov rdi,rax
  xor rsi,rsi
  mov rdx,256
  call aocc_memset

loop0:
  mov rdi,rsp
  mov rsi,72
  mov rdx,r15
  call aocc_fgets
  cmp rax,0
  je loop0end
  cmp byte ptr[rsp],'\n'
  je loop0end

  xor rbp,rbp
  mov rcx,32
loop1:
  movzx rax,byte ptr[rsp+rcx-1]
  cmp rax,'#'
  mov rax,0
  sete al
  shl rbp
  or rbp,rax

  dec rcx
  jnz loop1
loop1end:

  mov [r13],ebp
  add r13,4

  jmp loop0
loop0end:

  # transpose bits
  lea rdi,[r14+128]
  mov rsi,r14
  call transpose_bits

  mov rax,r14

ret0:
  add rsp,72
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

# (uint32_t* dst, uint32_t* src) -> void
transpose_bits:
  push r8
  push r9

  mov r8,0 # outer loop counter
loop3:
  mov r9,0 # inner loop counter
  xor rdx,rdx # accumulator
loop4:
  mov eax,[rsi+r9*4]
  mov rcx,r8
  shr eax,cl
  and eax,1
  mov rcx,r9
  shl eax,cl
  or edx,eax

  inc r9
  cmp r9,32
  jl loop4
loop4end:
  mov [rdi+r8*4],edx

  inc r8
  cmp r8,32
  jl loop3
loop3end:

  pop r9
  pop r8
  ret

# (uint32_t*, uint32_t) -> uint64_t
has_refl:
  push rbx
  xor rbx,rbx
  mov rcx,rsi # lower
  lea rdx,[rsi+1] # upper
  mov rax,727
loop5:
  cmp dword ptr[rdi+rcx*4],0
  je loop5end
  cmp dword ptr[rdi+rdx*4],0
  je loop5end

  mov eax,[rdi+rcx*4]
  xor eax,[rdi+rdx*4]
  popcnt eax,eax
  add rbx,rax
  xor rax,rax
  
  inc rdx
  dec rcx
  cmp rcx,0
  jge loop5
loop5end:
  cmp rax,0
  cmove rax,rbx
  pop rbx
  ret

# (uint32_t*) -> uint32_t
single_pattern_13b:
  push rbp
  push r8
  mov rbp,rdi

  # horizontal refl
  xor r8,r8
loop6:
  mov rdi,rbp
  mov rsi,r8
  call has_refl
  xor rdi,rdi
  cmp rax,1
  sete dil

  lea rax,[r8+1]
  imul rax,rax,100

  cmp dil,0
  jne ret1

  inc r8
  cmp r8,31
  jl loop6
loop6end:

  # vertical refl
  xor r8,r8
loop7:
  lea rdi,[rbp+128]
  mov rsi,r8
  call has_refl
  xor rdi,rdi
  cmp rax,1
  sete dil

  lea rax,[r8+1]

  cmp dil,0
  jne ret1
  
  inc r8
  cmp r8,31
  jl loop7
loop7end:

  call die
ret1:
  pop r8
  pop rbp
  ret

.global p13b
p13b:
  push rbp # accumulator
  push rbx
  push r8
  push r9
  push r10
  push r11
  push r12
  push r13
  push r14 # obj
  push r15 # file
  sub rsp,8

  mov r15,rdi
  xor rbp,rbp
  
loop8:
  mov rdi,r15
  call parse_13b
  cmp rax,0
  je loop8end
  mov r14,rax

  mov rdi,r14
  call single_pattern_13b
  add rbp,rax

  mov rdi,r14
  call aocc_free
  jmp loop8
loop8end:

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
die:
  ud2