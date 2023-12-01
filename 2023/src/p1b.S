.intel_syntax noprefix

lut:
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "one\0\0\0\0\0"
.ascii "\xff\xff\xff\0\0\0\0\0"
.ascii "two\0\0\0\0\0"
.ascii "\xff\xff\xff\0\0\0\0\0"
.ascii "three\0\0\0"
.ascii "\xff\xff\xff\xff\xff\0\0\0"
.ascii "four\0\0\0\0"
.ascii "\xff\xff\xff\xff\0\0\0\0"
.ascii "five\0\0\0\0"
.ascii "\xff\xff\xff\xff\0\0\0\0"
.ascii "six\0\0\0\0\0"
.ascii "\xff\xff\xff\0\0\0\0\0"
.ascii "seven\0\0\0"
.ascii "\xff\xff\xff\xff\xff\0\0\0"
.ascii "eight\0\0\0"
.ascii "\xff\xff\xff\xff\xff\0\0\0"
.ascii "nine\0\0\0\0"
.ascii "\xff\xff\xff\xff\0\0\0\0"

# (char*) -> int
# 0 if no word
match_word:
  # rax - loaded
  # rdi - pattern
  # rsi - mask
  # rcx - count
  # rdx - scratch
  mov rax,qword ptr[rdi]
  mov rcx,9
loop2:
  mov rdx,rcx
  shl rdx,4 # *16
  mov rdi,[lut+rdx]
  mov rsi,[lut+rdx+8]

  mov rdx,rax
  and rdx,rsi
  xor rdx,rdi
  jnz br3

  # found it, retn
  mov rax,rcx
  ret
br3:

  dec rcx
  jnz loop2
  
  xor eax,eax
  ret

.global p1b
p1b:
  # rdx - scratch
  push rbp # first
  push rbx # last
  push r12 # sum
  push r13 # curr line
  push r14 # FILE* input
  push r15 # pos in curr line

  mov r14,rdi

  mov rdi,4096
  call malloc
  mov r13,rax

  xor rax,rax
  xor r12,r12
  xor rbp,rbp
  xor rbx,rbx
loop0:

  xor r15,r15
  mov rdi,r13
  mov rsi,4096
  mov rdx,r14
  call fgets
  cmp rax,0
  jz loop0br

loop1:
  xor rax,rax
  mov al,byte ptr[r13+r15]
  movzx rax,al

  cmp rax,'0'
  jl br2
  cmp rax,'9'
  jg br2
  
  # digit
  cmp rbp,0
  cmove rbp,rax
  mov rbx,rax
  jmp br4

br2: 
  # letter
  lea rdi,[r13+r15]
  call match_word
  cmp rax,0
  jz br4

  # char-ify it
  add rax,'0'
  cmp rbp,0
  cmove rbp,rax
  mov rbx,rax  

br4:

  inc r15
  cmp byte ptr[r13+r15],0
  jne loop1

  # process rbp/rbx and add to cnt

  sub rbp,'0'
  sub rbx,'0'

  push rax
  mov rax,10
  mul rbp
  add rbx,rax
  pop rax
  add r12,rbx
  
  xor rbp,rbp
  xor rbx,rbx

  jmp loop0
loop0br:

  mov rax,r12

  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx
  pop rbp
  ret