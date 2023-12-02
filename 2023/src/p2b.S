.intel_syntax noprefix
#include "lib.h.S"

# format: 4b nr, 4b ng, 4b nb

# (FILE*) -> struct*
parse_in:
  # rax: scratch
  # rcx: scratch
  push rbp 
  push rbx # pos within r14
  push r12 # loop0 counter
  push r13 # curr line
  push r14 # parsed input buf
  push r15 # FILE* input
  sub rsp,8

  mov r15,rdi
  mov rdi,1024
  call malloc
  mov r13,rax
  mov rdi,1200
  call malloc
  mov r14,rax

  xor r12,r12
  mov rbx,r14
loop0:
  inc r12
  # getline
  mov rdi,r13
  mov rsi,1024
  mov rdx,r15
  call fgets
  cmp rax,0
  jz loop0br

  # parse line
  mov rcx,r13
  # rcx: stringstream ptr
  # rax: char
  # rdx: curr num
  # wait till colon
loop1:
  movzx rax,byte ptr[rcx]
  cmp rax,':'
  je loop1br
  inc rcx
  jmp loop1
loop1br:

loop2:
  movzx rax,byte ptr[rcx]

  cmp rax,0
  je loop2br

  cmp rax,'0'
  jl br0a
  cmp rax,'9'
  jg br0a

  # isdigit
  push rax
  push rcx
  sub rsp,16

  mov rdi,rcx
  mov rsi,rsp
  mov rdx,10
  call strtol
  mov rdx,rax

  add rsp,16
  pop rcx
  pop rax
  mov rcx,[rsp-32] # read endptr
  jmp br0end
br0a:

  # ignore color code if not preceded by space
  cmp byte ptr[rcx-1],' '
  jne br0end

  cmp rax,'r'
  jne br0b
  mov eax,[rbx]
  cmp rax,rdx
  cmovl rax,rdx
  mov [rbx],eax
  jmp br0end
br0b:
  
  cmp rax,'g'
  jne br0c
  mov eax,[rbx+4]
  cmp rax,rdx
  cmovl rax,rdx
  mov [rbx+4],eax
  jmp br0end
br0c:

  cmp rax,'b'
  jne br0end
  mov eax,[rbx+8]
  cmp rax,rdx
  cmovl rax,rdx
  mov [rbx+8],eax
br0end:

  inc rcx
  jmp loop2
loop2br:

  add rbx,12
  jmp loop0
loop0br:

  # free
  mov rdi,r13
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

.global p2b
p2b:
  push rbp 
  push rbx 
  push r12
  push r13 # loop cnt 100->1
  push r14 # sum
  push r15 # parsed input buf
  sub rsp,8

  call parse_in
  mov r15,rax
  xor r14,r14
  mov r13,100 # hardcoded input sz

loop3:
  # rcx: curr line
  mov rcx,r13
  dec rcx
  imul rcx,12
  add rcx,r15

  mov edi,dword ptr[rcx]
  mov eax,dword ptr[rcx+4]
  imul rdi,rax
  mov eax,dword ptr[rcx+8]
  imul rdi,rax
  add r14,rdi

  dec r13
  jnz loop3
loop3end:

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