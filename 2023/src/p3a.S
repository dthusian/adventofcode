.intel_syntax noprefix

#define STRIDE 141

str0:
  .ascii "................"
str1:
  .ascii "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"

masks:
  .ascii "\xff\x00\xff\x00\x00\x00\x00\x00"
  .ascii "\xff\xff\xff\x00\x00\x00\x00\x00"
  .ascii "\xff\x00\x00\xff\x00\x00\x00\x00"
  .ascii "\xff\xff\xff\xff\x00\x00\x00\x00"
  .ascii "\xff\x00\x00\x00\xff\x00\x00\x00"
  .ascii "\xff\xff\xff\xff\xff\x00\x00\x00"

# (FILE*) -> void*
parse_3a:
  push rbp 
  push rbx 
  push r12
  push r13 # FILE* in
  push r14 # obj
  push r15 # obj mut
  sub rsp,8

  mov r13,rdi

  mov rdi,131072
  call malloc
  mov r14,rax
  
  mov r15,r14 # add null padding
  add r15,STRIDE*2
loop0:
  mov rdi,r15
  mov rsi,1024
  mov rdx,r13
  call fgets
  cmp rax,0
  jz loop0end
  add r15,STRIDE
  mov byte ptr[r15-1],0
  jmp loop0
loop0end:

  mov rax,r14

  add rsp,8
  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx
  pop rbp
  ret

.global p3a
p3a:
  # rax: short-term scratch
  # r8: scratch
  # r9: scratch
  # r10: scratch
  push rbp 
  push rbx 
  push r12 # counter
  push r13 # current char
  push r14 # parsed mut
  push r15 # parsed
  sub rsp,8

  call parse_3a
  mov r15,rax
  mov r14,r15
  xor r12,r12

  add r14,STRIDE*2
loop1:
  movzx r13,byte ptr[r14]
  movzx rax,byte ptr[r14+1]
  or rax,r13
  jz loop1end # both zero -> two \0 consecutively -> exit

  # check if start of num
  movzx rax,byte ptr[r14-1]
  sub rax,'0'
  cmp rax,10
  jb br0end # cannot be preceded by digit
  mov rax,r13
  sub rax,'0'
  cmp rax,10
  ja br0end # must be digit
br0a:
  # check if is part no
  mov rdi,r14
  xor rsi,rsi
  mov rdx,10
  call strtol

  mov r8,[r14-1-STRIDE]
  mov r9,[r14-1]
  mov r10,[r14-1+STRIDE]
  
  cmp rax,99
  jg br1c
  cmp rax,9
  jg br1b
br1a: # one digit
  and r8,[masks+8]
  and r9,[masks]
  and r10,[masks+8]
  jmp br1end
br1b: # two digit
  and r8,[masks+24]
  and r9,[masks+16]
  and r10,[masks+24]
  jmp br1end
br1c: # three digit
  and r8,[masks+40]
  and r9,[masks+32]
  and r10,[masks+40]
br1end:

  # check for not-. not-0
  pxor xmm0,xmm0
  movq xmm0,r8
  movq xmm1,r9
  movq xmm2,r10
  pslldq xmm1,5
  pslldq xmm2,10
  por xmm0,xmm1
  por xmm0,xmm2
  # xmm0 now contains bytes to check
  movdqu xmm3,[str0]
  movdqu xmm4,[str1]
  pcmpeqb xmm3,xmm0
  pxor xmm3,xmm4 # now contains zeroes where there is .
  pand xmm0,xmm3

  xor rcx,rcx
  ptest xmm0,xmm0
  # . if zf = 0, then there is part nearby
  cmovnz rcx,rax
  add r12,rcx
br0end:

  inc r14
  jmp loop1
loop1end:

  mov rdi,r15
  call free

  mov rax,r12
  
  add rsp,8
  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx
  pop rbp
  ret