.intel_syntax noprefix

# struct index: char* ins, char* graph
# size: 16
# struct node: uint32_t src; uint32_t dst1; uint32_t dst2;
# size: 12

# (FILE*, void**) -> void
parse_8a:
  push rbp
  push rbx
  push r8
  push r9  
  push r10 # idx buf
  push r11 # ins buf
  push r12 # node buf mut
  push r13
  push r14 # curr line
  push r15 # file
  sub rsp,8

  mov r15,rdi
  mov r10,rsi

  mov rdi,1024
  call aocc_malloc
  mov r14,rax
  
  mov rdi,1024
  call aocc_malloc
  mov r11,rax
  
  mov rdi,16384
  call aocc_malloc
  mov r12,rax

  mov [r10],r11
  mov [r10+8],r12

  mov rdi,r11
  mov rsi,1024
  mov rdx,r15
  call aocc_fgets
  mov rdi,r15
  call aocc_getc

loop0:
  mov rdi,r14
  mov rsi,1024
  mov rdx,r15
  call aocc_fgets
  cmp rax,0
  je loop0end

  mov eax,[r14]
  and eax,0xffffff
  mov [r12],eax
  mov eax,[r14+7]
  and eax,0xffffff
  mov [r12+4],eax
  mov eax,[r14+12]
  and eax,0xffffff
  mov [r12+8],eax

  add r12,12
  jmp loop0
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

# (node*, uint32_t, bool dir) -> uint32_t
lookup_node:
loop2:
  cmp esi,[rdi]
  je loop2end
  add rdi,12
  jmp loop2
loop2end:
  mov eax,[rdi+4]
  mov ecx,[rdi+8]
  cmp edx,1
  cmove eax,ecx
  ret

.global p8a
p8a:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11 # steps taken
  push r12 # current node id
  push r13 # instr buf mut
  push r14 # instr buf
  push r15 # node buf
  sub rsp,24

  mov rsi,rsp
  call parse_8a

  mov r14,[rsp]
  mov r13,r14
  mov r15,[rsp+8]

  mov r12,0x414141 # "AAA"
  xor r11,r11
loop1:
  # restart instruction list if reaching end
  movzx rax,byte ptr[r13]
  cmp rax,0xa
  cmove r13,r14
  je loop1
  # set rdx = 1 if R
  xor rdx,rdx
  mov rcx,1
  cmp rax,'R'
  cmove rdx,rcx

  mov rdi,r15
  mov esi,r12d
  call lookup_node
  inc r11
  mov r12d,eax

  inc r13
  cmp r12,0x5a5a5a # "ZZZ"
  jne loop1
loop1end:

  mov rdi,r14
  call aocc_free
  mov rdi,r15
  call aocc_free

  mov rax,r11

  add rsp,24
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