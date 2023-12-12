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
  mov rdi,rax
  xor rsi,rsi
  mov rdx,16384
  call aocc_memset

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

# (instr buf*, node buf*, uint32_t start) -> uint64_t
simloop:
  push rbp # product
  push rbx # instr buf len
  push r8
  push r9
  push r10 # end
  push r11 # steps taken
  push r12 # current node id
  push r13 # instr buf mut
  push r14 # instr buf
  push r15 # node buf
  sub rsp,8

  mov r14,rdi
  mov r13,rdi
  mov r15,rsi
  mov r12,rdx
  mov r10,rcx
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
  mov rax,r12
  and rax,0xff0000
  cmp rax,0x5a0000 # "Z"
  jne loop1
loop1end:

  mov rax,r11

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

.global p8b
p8b:
  push rbp
  push rbx
  push r8
  push r9
  push r10 
  push r11
  push r12 # rsp mut
  push r13 # node buf mut
  push r14 # instr buf
  push r15 # node buf
  sub rsp,72

  mov rsi,rsp
  call parse_8a

  mov r14,[rsp]
  mov r15,[rsp+8]
  mov r13,r15

  mov r12,rsp
  mov rdi,rsp
  mov rsi,0
  mov rdx,72
  call aocc_memset
  # find starting points ending in A
loop3:
  mov edx,[r13]
  cmp edx,0
  je loop3end

  mov eax,edx
  shr eax,16
  cmp eax,'A'
  jne br0end
br0a:
  mov rdi,r14
  mov rsi,r15
  call simloop
  mov [r12],rax
  add r12,8
br0end:

  add r13,12
  jmp loop3
loop3end:

  mov rdi,r14
  call aocc_strlen
  mov rbx,rax
  dec rbx
  xor rbp,rbp
  inc rbp
  xor rsi,rsi
  inc rsi
  mov rcx,8
loop4:
  xor edx,edx
  pop rax
  idiv rbx
  cmp rax,0
  cmove rax,rsi
  imul rbp,rax
  dec rcx
  jnz loop4
loop4end:
  imul rbp,rbx

  mov rdi,r14
  call aocc_free
  mov rdi,r15
  call aocc_free

  mov rax,rbp

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