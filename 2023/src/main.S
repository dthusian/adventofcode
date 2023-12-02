.intel_syntax noprefix

.align 16
str0:
.asciz "%ld\n"
str1:
.asciz "input.txt"
str2:
.asciz "rb"
.global main
main:
  push rbx

  lea rdi,[str1]
  lea rsi,[str2]
  call fopen
  
  mov rdi,rax
  call p2b

  lea rdi,[str0]
  mov rsi,rax
  xor eax,eax
  call printf

  xor eax,eax
  pop rbx
  ret