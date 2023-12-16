.intel_syntax noprefix

# struct star { uint32_t x; uint32_t y; }

str0:
  .asciz "%d\n"

# (FILE*, uint64_t*) -> star*
parse_11a:
  push rbp
  push rbx
  push r8  # x
  push r9  # y
  push r10 # num objs out ptr
  push r11 # buf mut
  push r12 # buf
  push r13 # obj mut
  push r14 # obj
  push r15 # file
  sub rsp,8

  mov r15,rdi
  mov r10,rsi

  mov rdi,65536
  call aocc_malloc
  mov r14,rax
  mov r13,rax
  mov rdi,rax
  mov rsi,0
  mov rdx,65536
  call aocc_memset

  mov rdi,65536
  call aocc_malloc
  mov r12,rax
  mov r11,rax
  mov rdi,rax
  mov rsi,0
  mov rdx,65536
  call aocc_memset

  mov rdi,r12
  mov rsi,1
  mov rdx,65536
  mov rcx,r15
  call aocc_fread

  xor r8,r8
  xor r9,r9
loop0:
  movzx rax,byte ptr[r11]
  inc r11

  cmp rax,0
  je loop0end
  
  cmp rax,'\n'
  je br0a
  cmp rax,'#'
  je br0b
  jmp br0end
br0a:
  xor r8,r8
  inc r9
  jmp loop0
br0b:
  mov [r13],r8d
  mov [r13+4],r9d
  add r13,8
br0end:

  inc r8
  jmp loop0
loop0end:

  mov rdi,r12
  call aocc_free

  mov rax,r13
  sub rax,r14
  shr rax,3
  mov [r10],rax
  mov rax,r14

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

qsort_cmp_11a:
  mov eax,[rdi]
  mov esi,[rsi]
  sub eax,esi
  ret

# WARN: non-standard calling conv!
# r14: star*
# r13: last seen coord
# r12: current offset
# rdx: cobbled
# rcx: loop counter
# rax: cobbled
expand_universe:
  xor r12,r12
  xor r13,r13
loop1:
  mov edx,[r14]

  cmp edx,r13d
  je br1end # check if offset change
br1a:
  mov eax,edx
  sub eax,r13d
  dec eax
  imul eax,eax,999999
  add r12d,eax
br1end:

  mov r13d,[r14]
  # add offset
  add [r14],r12d

  add r14,8
  dec rcx
  jnz loop1
loop1end:
  ret

.global p11b
p11b:
  push rbp
  push rbx # sum
  push r8 # x
  push r9 # y
  push r10
  push r11 # obj len
  push r12 # current offset
  push r13 # last seen coord
  push r14 # obj mut
  push r15 # obj
  sub rsp,8

  mov rsi,rsp
  call parse_11a
  mov r15,rax
  mov r11,[rsp]

  # galaxies will be y-sorted right here
  lea r14,[r15+4]
  mov rcx,r11
  call expand_universe

  # x-sort galaxies
  mov rdi,r15
  mov rsi,r11
  mov rdx,8
  lea rcx,[qsort_cmp_11a]
  call aocc_qsort

  mov r14,r15
  mov rcx,r11
  call expand_universe

  # sum galaxies

  mov r14,r15
  xor r8,r8
  xor r9,r9
  xor rbx,rbx
  # r8,9 are loop counters
loop2: # r8 loop
  xor r9,r9

loop3: # r9 loop
  mov edi,[r15+r8*8]
  mov esi,[r15+r9*8]
  sub edi,esi
  mov esi,edi
  neg edi
  cmovl edi,esi
  add rbx,rdi

  mov edi,[r15+r8*8+4]
  mov esi,[r15+r9*8+4]
  sub edi,esi
  mov esi,edi
  neg edi
  cmovl edi,esi
  add rbx,rdi

  inc r9
  cmp r9,r11
  jl loop3
loop3end:

  inc r8
  cmp r8,r11
  jl loop2
loop2end:
  shr rbx,1

  mov rdi,r15
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
die:
  ud2