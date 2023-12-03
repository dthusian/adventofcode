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

.global p3b
p3b:
  # rax: short-term scratch
  push rbp # product of found part nos
  push rbx # (loop2) loop counter
  push r12 # counter
  push r13 # current char / (loop2) index of last seen part no
  push r14 # parsed mut
  push r15 # (loop2) number of successful parses
  sub rsp,8 # parsed

  call parse_3a
  mov [rsp],rax
  mov r14,rax
  xor r12,r12

  add r14,STRIDE*2
loop1:
  movzx r13,byte ptr[r14]
  movzx rax,byte ptr[r14+1]
  or rax,r13
  jz loop1end # both zero -> two \0 consecutively -> exit

  cmp r13,'*'
  jne br0end
br0a:

  # setup loop
  
  # stack array
  mov rax,STRIDE+1
  neg rax
  push 0
  push rax

  mov rax,STRIDE
  neg rax
  push 0
  push rax

  mov rax,STRIDE-1
  neg rax
  push 0
  push rax

  mov rax,1
  neg rax
  push 0
  push rax

  mov rax,1
  push 0
  push rax

  mov rax,STRIDE-1
  push 0
  push rax

  mov rax,STRIDE
  push 0
  push rax

  mov rax,STRIDE+1
  push 0
  push rax

  mov rbp,1 # product
  mov rbx,8 # loop counter
  xor r13,r13 # last scanned index
  xor r15,r15 # number of successful scans
loop2:
  # rdx: current ptr
  pop rdx
  pop rdi # recieve the stack padding 0
  add rdx,r14

  # test if digit
  movzx rax,byte ptr[rdx]
  cmp rax,'0'
  jl br1end
  cmp rax,'9'
  jg br1end

  # scan until not digit
loop3:
  movzx rax,byte ptr[rdx]
  cmp rax,'0'
  jl loop3end
  cmp rax,'9'
  jg loop3end
  dec rdx
  jmp loop3
loop3end:
  inc rdx

  # check if that int hasnt already been scanned
  cmp r13,0
  je br1a
  cmp r13,rdx
  je br1end # already found, do nothing
br1a:
  mov r13,rdx
  # read int
  mov rdi,rdx
  xor rsi,rsi
  mov rdx,10
  call strtol
  
  imul rbp,rax
  inc r15
  
br1end:

  dec rbx
  jnz loop2
loop2end:
  
  # only add if 2 values were scanned (3 is assertion failure)
  cmp r15,2
  jl br0end
  jg die
  add r12,rbp

br0end:

  inc r14
  jmp loop1
loop1end:

  mov rdi,[rsp]
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

die:
  ud2