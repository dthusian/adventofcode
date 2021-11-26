  bits 64
section .text
  extern fopen
  extern fclose
  extern fgetc
  extern putc
  extern printf
  extern malloc
  extern realloc
  extern memcpy
  extern free
  extern exit

  global a8_crash
  global a8_exit

  global a8_arr_push1

  global a8_io_init
  global a8_io_read_until
  global a8_io_print_long
  global a8_io_writec

;;; Section: General Utilities ;;;
; This library provides essential functions like
; a8_exit() and a8_crash().

a8_crash: ; void (*)();
  mov rdi,1
  call exit
  ret

a8_exit: ; void (*)(int);
  call exit
  ret

;;; Section: Sequences Library ;;;
; This library provides functions to manipulate
; sequences and strings.

; struct a8_arr {
;   uint32_t sz_used;
;   uint32_t sz_total;
;   void* data;
; }

a8_arr_init: ; a8_arr* (*)();
  ; locals:
  ; r12 = arr_array
  ; r13 = stack alignment
  push r12
  push r13

  mov rdi,16
  call malloc
  mov r12,rax
  mov dword [r12],0
  mov dword [r12+4],32

  mov rdi,32
  call malloc
  mov qword [r12+8],rax

  pop r13
  pop r12
  ret

a8_arr_concat: ; void (*)(a8_arr*, a8_arr*);
  ; locals:
  ; r12 = dest_arr
  ; r13 = src_arr
  push r12
  push r13

  mov r12,rdi
  mov r13,rsi

  xor r8d,r8d
  add r8d,dword [r12]
  add r8d,dword [r13]

  cmp r8d,dword [r12+4]
  jbe LBL_4

LBL_5:
  mov rdi,r12
  call a8_arr_expand

  cmp r8d,dword [r12+4]
  ja LBL_5

LBL_4:
  mov rdi,[r12+8]
  add rdi,[r12]
  mov rsi,[r13+8]
  xor edx,edx
  mov edx,dword [r13]
  call memcpy

  xor r8d,r8d
  mov r8d,dword [r13]
  add dword [r12],r8d

  pop r13
  pop r12
  ret

a8_arr_push1: ; void (*)(a8_arr*, int8_t)
  xor r8d,r8d
  mov r8d,dword [rdi]
  add r8d,1
  cmp r8d,dword [rdi+4]
  jbe LBL_6
  push rdi
  push rsi
  call a8_arr_expand
  pop rsi
  pop rdi
LBL_6:
  mov r8,qword [rdi+8]
  xor r9d,r9d
  mov r9d,dword [rdi]
  mov byte [r8+r9],sil
  inc dword [rdi]
  ret

a8_arr_expand: ; void (*)(a8_arr*)
  xor esi,esi
  mov esi,dword [rdi+4]
  lea esi,[esi*2-16]
  mov dword [rdi+4],esi
  mov rdi,qword [rdi+8]
  call realloc
  ret

;;; Section: String Library ;;;
; This section has methods that operate on
; character arrays.

; (nothing here yet)

;;; Section: I/O Library ;;;
; This library provides convenience functions for 
; reading from the input file and outputting
; debug messages and the answer.

a8_io_init: ; void (*)();
  lea rdi,[rel STATIC_1]
  lea rsi,[rel STATIC_2]
  call fopen
  mov qword [rel fp],rax
  ret
section .data
  STATIC_1 db "input.txt",0x0
  STATIC_2 db "r",0x0
section .bss
  fp resq 1
section .text

a8_io_read_until: ; a8_arr* (*)(char);
  ; locals:
  ; r12 = buf
  ; r13 = until_char
  push r12
  push r13

  mov r13,rdi

  call a8_arr_init
  mov r12,rax

LBL_1:
  mov rdi,qword [rel fp]
  call fgetc

  cmp eax,0 ; eof check
  jle LBL_2

  cmp rax,r13 ; char check
  je LBL_2

  mov rdi,rax
  call a8_arr_push1

  jmp LBL_1
LBL_2:
  mov rax,r12

  pop r13
  pop r12
  ret

a8_io_writec:
  call putc
  ret

a8_io_print_long: ; void (*)(int64_t);
  push rdi
  xor eax,eax
  lea rdi,[rel STATIC_5]
  pop rsi
  call printf
  ret
section .data
  STATIC_5 db "%d",0xa,0x0
section .text

a8_io_print_str: ; void (*)(char*);
  push rdi
  xor eax,eax
  lea rdi,[rel STATIC_6]
  pop rsi
  call printf
  ret
section .data
  STATIC_6 db "%s",0xa,0x0
section .text