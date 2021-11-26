  bits 64
section .text
  global _start
  extern a8_arr_push1
  extern a8_io_init
  extern a8_io_read_until
  extern a8_io_print_long
  extern a8_io_writec
  extern a8_exit

_start:
  push r12
  push r13
  call a8_io_init

  mov rdi,0
  call a8_io_read_until
  mov r12,rax ; a8_arr*
  mov r13,qword [rax+8] ; void*

  mov rdi,r12
  movzx rsi,byte [r13]
  call a8_arr_push1
  dec dword [r12]

  xor r9d,r9d
  xor r8d,r8d ; loop counter
LBL_1:
  movzx rdi,byte [r13+r8]
  movzx rsi,byte [r13+r8+1]
  cmp rdi,rsi
  jne LBL_2
  lea r9,[r9+rdi-'0']
LBL_2:
  inc r8
  cmp r8d,dword [r12]
  jl LBL_1

  mov rdi,r9
  call a8_io_print_long
  mov rdi,0xa
  call a8_io_writec 
  mov rdi,0
  call a8_exit