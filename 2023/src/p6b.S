.intel_syntax noprefix

# __aocc (FILE*, uint64_t*) -> void 
parse_6b:
  push rbp
  push rbx
  push r8
  push r9
  push r10 # buf
  push r11 # filebuf mut
  push r12 # n2
  push r13 # n1
  push r14 # filebuf
  push r15 # file
  sub rsp,8

  xor r12,r12
  mov r15,rdi
  mov r10,rsi

  mov rdi,1024
  call aocc_malloc
  mov r14,rax
  mov r11,rax
  mov rdi,rax
  mov rsi,1024
  mov rdx,1
  mov rcx,r15
  call aocc_fread

loop0:
  movzx rax,byte ptr[r11]
  cmp rax,0
  je loop0end

  # swap the regs when nl
  xor rcx,rcx
  cmp rax,'\n'
  cmove r13,r12
  cmove r12,rcx

  # read into n2
  cmp rax,'0'
  jl br0end
  cmp rax,'9'
  jg br0end
br0a:
  sub rax,'0'
  mov rcx,10
  imul r12,rcx
  add r12,rax
br0end:
  
  inc r11
  jmp loop0
loop0end:

  mov [r10],r13
  mov [r10+8],r12

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

  .align 32
str0:
  .quad 0
  .quad 1
  .quad 2
  .quad 3
str1:
  .quad 1
  .quad 1
  .quad 1
  .quad 1

.global p6b
p6b:
  push rbx
  sub rsp,16
  mov rsi,rsp
  call parse_6b
  mov rdi,[rsp]
  mov rsi,[rsp+8]
  add rsp,16

  # rdi,rsi have the numbers
  # d < x*(t-x)

  xor ecx,ecx # counter
  vpxor ymm0,ymm0,ymm0 # score
  vmovdqa ymm1,[str0] # ymm1 = staircase
  vmovq xmm2,rdi
  vpbroadcastq ymm2,xmm2 # ymm2 = t
  vmovq xmm3,rsi
  vpbroadcastq ymm3,xmm3 # ymm3 = d
  vmovdqa ymm4,[str1] # ymm4 = 1
loop1:
  vmovq xmm8,rcx
  vpbroadcastq ymm8,xmm8
  vpaddq ymm8,ymm8,ymm1 # ymm8 = x
  vpsubq ymm9,ymm2,ymm8 # ymm9 = t-x
  vpmuldq ymm8,ymm8,ymm9 # ymm8 = x*(t-x)
  vpcmpgtq ymm8,ymm8,ymm3 # ymm8 = x*(t-x)>d
  vpand ymm8,ymm8,ymm4 # set to 1 if true
  vpaddq ymm0,ymm0,ymm8

  add rcx,4
  cmp rcx,rdi
  jl loop1
loop1end:

  sub rsp,32
  vmovdqu [rsp],ymm0
  xor eax,eax
  add rax,[rsp]
  add rax,[rsp+8]
  add rax,[rsp+16]
  add rax,[rsp+24]
  add rsp,32

  pop rbx
  vzeroupper
  ret