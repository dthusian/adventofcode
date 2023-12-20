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
  
  mov rbx,rax
  mov rdi,rax
  call p14b

  lea rdi,[str0]
  mov rsi,rax
  xor eax,eax
  call printf

  mov rdi,rbx
  call fclose

  xor eax,eax
  pop rbx
  ret