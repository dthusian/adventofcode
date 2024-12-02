.intel_syntax noprefix

#define BLOCK_SIZE 100

# typedef lens block[BLOCK_SIZE];
# struct lens { char name[7]; char focus; }

# struct parse_out { char name[7]; char command[2]; }

# (file*, parse_out*) -> void
parse_next_15b:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11 # output buf command mut
  push r12 # output buf name mut
  push r13
  push r14 # curr hash
  push r15 # file
  sub rsp,8

  mov r15,rdi
  mov r12,rsi
  mov r11,rsi
  xor r14,r14
loop0:
  mov rdi,r15
  call aocc_getc

  cmp rax,-1
  je loop0end
  cmp rax,'\n'
  je loop0end
  cmp rax,0
  je loop0end
  cmp rax,','
  je loop0end

  cmp rax,'a'
  jl br0b
  cmp rax,'z'
  jg br0b
br0a:
  # part of name
  mov [r12],al
  inc r12
br0b:
  # part of command
  mov [r11],al
  inc r11
br0end:

  add r14,rax
  imul r14,r14,17
  and r14,0xff

  jmp loop0end
loop0end:

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

.global p15b
p15b:
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