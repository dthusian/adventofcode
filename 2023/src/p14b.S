.intel_syntax noprefix

#define STRIDE 128

# y*STRIDE+x
# typdef char obj[STRIDE*STRIDE]

# (file*) -> obj*
parse_14b:
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
  mov rdi,STRIDE*STRIDE
  call aocc_malloc
  mov r14,rax
  mov r13,rax
  mov rdi,rax
  mov rsi,0
  mov rdx,STRIDE*STRIDE
  call aocc_memset

loop0:
  mov rdi,r13
  mov rsi,128
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

.macro IDX2MOD tgt,x,y
  test rsi,2
  jnz idx2_br0_m\@
  # non-transposed
  imul \tgt,\y,STRIDE
  add \tgt,\x
  jmp idx2_br1_m\@
idx2_br0_m\@:
  # transposed
  imul \tgt,\x,STRIDE
  add \tgt,\y
idx2_br1_m\@:
.endm

.macro IDX2 tgt,x,y
  imul \tgt,\y,STRIDE
  add \tgt,\x
.endm

# (obj*, int) -> void
# 0 = up, 1 = down, 2 = left, 3 = right
roll_14b:
  push rbx # curr char
  push r8 # x
  push r9 # y
  push r10 # scratch
  push r11 # scratch
  # iter columns
  xor r8,r8
loop1:
  # setup col vars
  xor r9,r9
  xor rcx,rcx # idx start of sector 
  xor rdx,rdx # num rocks in sector
  # iter in a col
loop2:
  IDX2MOD rax,r8,r9
  movzx rbx,byte ptr[rdi+rax]
  cmp rbx,'.'
  je br2end
  cmp rbx,'O'
  je br2a
  # blocker
  cmp rdx,0 # no rocks in this sector (optimization+correctness for 0-sized sectors)
  je br2d
  test rsi,1
  jz br2b
  jmp br2c
br2a: # rock
  inc rdx
  jmp br2end
br2b: # blocker, up
  
loop3:
  mov r10,'.'
  mov rax,'O'
  dec rdx
  cmovge r10,rax
  IDX2MOD rax,r8,rcx
  mov [rdi+rax],r10b
  inc rcx
  cmp rcx,r9
  jl loop3
loop3end:
  inc rcx
  xor rdx,rdx

  jmp br2end
br2c: # blocker, down

  # rdx = -((r9 - rcx) - rdx)
  sub rdx,r9
  add rdx,rcx
loop4:
  mov r10,'.'
  mov rax,'O'
  inc rdx
  cmovg r10,rax
  IDX2MOD rax,r8,rcx
  mov [rdi+rax],r10b
  inc rcx
  cmp rcx,r9
  jl loop4
loop4end:
  inc rcx
  xor rdx,rdx

  jmp br2end
br2d:
  lea rcx,[r9+1]
br2end:

  inc r9
  cmp r9,STRIDE
  jl loop2
loop2end:

  inc r8
  cmp r8,STRIDE
  jl loop1
loop1end:

  xor eax,eax
  pop r11
  pop r10
  pop r9
  pop r8
  pop rbx
  ret
.endm

# (obj*) -> uint32_t
calc_load:
  push rbx
  # find bottom edge
  xor rsi,rsi
loop5:
  imul rax,rsi,128
  movzx rax,byte ptr[rdi+rax+1]
  cmp rax,0
  je loop5end
  inc rsi
  jmp loop5
loop5end:
  mov rdx,rsi # bottom edge

  # calc load
  xor rsi,rsi
  xor rbx,rbx
loop6:
  movzx rax,byte ptr[rdi+rsi]
  cmp rax,'O'
  jne br3end
br3a:
  # find row
  mov ecx,STRIDE
  tzcnt ecx,ecx
  mov rax,rsi
  shr rax,cl
  # sub row from bottom
  mov rcx,rdx
  sub rcx,rax
  # add to sum
  add rbx,rcx
br3end:
  inc rsi
  cmp rsi,STRIDE*STRIDE
  jl loop6
loop6end:
  mov eax,ebx
  pop rbx
  ret

# (obj*) -> uint32_t
calc_hash:
  xor rsi,rsi
  mov rax,727727727
loop7:
  crc32 rax,qword ptr[rdi+rsi]
  add rsi,8
  cmp rsi,STRIDE*STRIDE
  jl loop7
loop7end:
  ret

.macro debugprint
  xor eax,eax
  lea rdi,[r8]
  call aocc_printf
  lea rdi,[r8+128]
  call aocc_printf
  lea rdi,[r8+128*2]
  call aocc_printf
  lea rdi,[r8+128*3]
  call aocc_printf
  lea rdi,[r8+128*4]
  call aocc_printf
  lea rdi,[r8+128*5]
  call aocc_printf
  lea rdi,[r8+128*6]
  call aocc_printf
  lea rdi,[r8+128*7]
  call aocc_printf
  lea rdi,[r8+128*8]
  call aocc_printf
  lea rdi,[r8+128*9]
  call aocc_printf
.endm
# (obj*) -> void
single_cycle:
  push r8
  mov r8,rdi
  mov rsi,0
  call roll_14b
  # debugprint
  mov rdi,r8
  mov rsi,2
  call roll_14b
  # debugprint
  mov rdi,r8
  mov rsi,1
  call roll_14b
  # debugprint
  mov rdi,r8
  mov rsi,3
  call roll_14b
  # debugprint
  pop r8
  ret

# (history*, uint32_t k) -> uint64_t
# ret -1 if not found
lin_search:
  xor rcx,rcx
  xor rdx,rdx
  dec rdx
loop9:
  mov eax,[rdi+rcx*4]
  cmp eax,0
  je ret0
  cmp eax,esi
  je loop9end

  inc rcx
  jmp loop9
loop9end:
  mov rdx,rcx
ret0:
  mov rax,rdx
  ret

str0:
  .asciz "loop found: start: %d len: %d\n"

.global p14b
p14b:
  push rbp
  push rbx # count
  push r8
  push r9
  push r10
  push r11 # loop length
  push r12 # iter number
  push r13 # history buf mut
  push r14 # history buf
  push r15 # obj
  sub rsp,8

  call parse_14b
  mov r15,rax

  # history buf
  mov rdi,80000
  call aocc_malloc
  mov r14,rax
  mov r13,rax
  mov rdi,rax
  mov rsi,0
  mov rdx,80000
  call aocc_memset

  # detect cycles
  xor r12,r12
loop8:
  # simulate
  mov rdi,r15
  call single_cycle
  # calc hash
  mov rdi,r15
  call calc_hash
  mov [r13],eax
  add r13,4
  # check for loops
  mov rdi,r14
  mov esi,eax
  call lin_search
  cmp rax,r12
  jne loop8end
  # next
  inc r12
  jmp loop8
loop8end:

  # loop found
  mov r11,r12
  sub r11,rax
  lea rdi,[str0]
  mov rsi,r12
  mov rdx,r11
  xor eax,eax
  call aocc_printf
  inc r12
  
  # sim loops
loop10:
  add r12,r11
  cmp r12,1000000000
  jl loop10
loop10end:
  sub r12,r11
  sub r12,r11

  # sim last part
loop11:
  mov rdi,r15
  call single_cycle
  inc r12
  cmp r12,1000000000
  jl loop11
loop11end:

  mov rdi,r15
  call calc_load
  mov rbx,rax

  mov rdi,r15
  call aocc_free
  mov rdi,r14
  call aocc_free

  mov rax,rbx

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