.intel_syntax noprefix

#define STRIDE 256

parse_10a:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11
  push r12
  push r13 # obj mut
  push r14 # obj
  push r15 # file
  sub rsp,8

  mov r15,rdi

  mov rdi,65536
  call aocc_malloc
  mov r14,rax
  mov r13,rax
  mov rdi,rax
  mov rsi,0
  mov rdx,65536
  call aocc_memset

loop0:
  mov rdi,r13
  mov rsi,STRIDE
  mov rdx,r15
  call aocc_fgets
  cmp rax,0
  je loop0end

  add r13,STRIDE
  jmp loop0
loop0end:

  mov rax,r14

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

.macro moveup
  mov r12,0
  sub r14,STRIDE
.endm

.macro moveright
  mov r12,1
  inc r14
.endm

.macro movedown
  mov r12,2
  add r14,STRIDE
.endm

.macro moveleft
  mov r12,3
  dec r14
.endm

.global p10a
p10a:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11
  push r12 # last dir 0 = u 1 = r 2 = d 3 = l
  push r13 # steps
  push r14 # ptr
  push r15 # obj
  sub rsp,8

  call parse_10a
  mov r15,rax

  xor r14,r14
  xor r13,r13
  # scan for S
loop1:
  inc r14
  movzx rax,byte ptr[r15+r14]
  cmp rax,'S'
  jne loop1
loop1end:

  # simulate loop
loop2:
  movzx rax,byte ptr[r15+r14]
  cmp rax,'S'
  je br0a
  cmp rax,'-'
  je br0b
  cmp rax,'|'
  je br0c
  cmp rax,'L'
  je br0d
  cmp rax,'7'
  je br0e
  cmp rax,'F'
  je br0f
  cmp rax,'J'
  je br0g
  call die
br0a: # S
  cmp r13,0 # break if found start not at start
  jne loop2end
  # scan nearby tiles for connecting pipes
  lea rdx,[r15+r14]

  cmp byte ptr[rdx+STRIDE],'|'
  je br1a
  cmp byte ptr[rdx+STRIDE],'L'
  je br1a
  cmp byte ptr[rdx+STRIDE],'J'
  je br1a

  cmp byte ptr[rdx-STRIDE],'|'
  je br1b
  cmp byte ptr[rdx-STRIDE],'F'
  je br1b
  cmp byte ptr[rdx-STRIDE],'7'
  je br1b

  cmp byte ptr[rdx+1],'-'
  je br1c
  cmp byte ptr[rdx+1],'J'
  je br1c
  cmp byte ptr[rdx+1],'7'
  je br1c

  cmp byte ptr[rdx-1],'-'
  je br1d
  cmp byte ptr[rdx-1],'L'
  je br1d
  cmp byte ptr[rdx-1],'F'
  je br1d

  call die
br1a: # connects down
  MOVEDOWN
  jmp br0end
br1b: # connects up
  MOVEUP
  jmp br0end
br1c: # connects right
  MOVERIGHT
  jmp br0end
br1d: # connects up
  MOVELEFT
  jmp br0end
br1end:

br0b: # -
  cmp r12,1
  je br2a
  cmp r12,3
  je br2b
  call die
br2a:
  MOVERIGHT
  jmp br0end
br2b:
  MOVELEFT
  jmp br0end
br2end:

br0c: # |
  cmp r12,0
  je br3a
  cmp r12,2
  je br3b
  call die
br3a:
  MOVEUP
  jmp br0end
br3b:
  MOVEDOWN
  jmp br0end
br3end:

br0d: # L
  cmp r12,2
  je br4a
  cmp r12,3
  je br4b
  call die
br4a:
  MOVERIGHT
  jmp br0end
br4b:
  MOVEUP
  jmp br0end
br4end:

br0e: # 7
  cmp r12,1
  je br5a
  cmp r12,0
  je br5b
  call die
br5a:
  MOVEDOWN
  jmp br0end
br5b:
  MOVELEFT
  jmp br0end
br5end:

br0f: # F
  cmp r12,0
  je br6a
  cmp r12,3
  je br6b
  call die
br6a:
  MOVERIGHT
  jmp br0end
br6b:
  MOVEDOWN
  jmp br0end
br6end:

br0g: # J
  cmp r12,2
  je br7a
  cmp r12,1
  je br7b
  call die
br7a:
  MOVELEFT
  jmp br0end
br7b:
  MOVEUP
  jmp br0end
br7end:

br0end:

  inc r13
  jmp loop2
loop2end:

  shr r13,1
  mov rax,r13

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
die:
  ud2