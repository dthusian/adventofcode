.intel_syntax noprefix

#define LEN 512

str0:
  .ascii "   @@   "
  .ascii " @    @ "
  .ascii "    @ @ "
  .ascii "   @  @ "
  .ascii " @ @    "
  .ascii " @  @   "
  .ascii " @ @@ @ "
str1:
  .ascii "-|F7JLS"

parse_10b:
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

  mov rdi,LEN*LEN
  call aocc_malloc
  mov r14,rax
  mov r13,rax
  mov rdi,rax
  mov rsi,0
  mov rdx,LEN*LEN
  call aocc_memset

loop0:
  mov rdi,r13
  mov rsi,LEN
  mov rdx,r15
  call aocc_fgets
  cmp rax,0
  je loop0end

  add r13,LEN
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

# (byte* ptr, uint64_t idx, uint64_t mask) -> void
put_tile:
  imul rsi,rsi,3
  add rdi,rsi

  mov [rdi],dl
  shr rdx,8
  mov [rdi+1],dl
  shr rdx,8
  mov [rdi+2],dl
  shr rdx,8
  mov [rdi+LEN],dl
  shr rdx,8

  mov byte ptr[rdi+LEN+1],'@'

  mov [rdi+LEN+2],dl
  shr rdx,8
  mov [rdi+LEN*2],dl
  shr rdx,8
  mov [rdi+LEN*2+1],dl
  shr rdx,8
  mov [rdi+LEN*2+2],dl
  ret

.macro dfs_floodfill_movcheck op,oparg
  mov rdi,[rsp+8]
  mov rsi,[rsp]
  \op rsi,\oparg
  cmp rsi,0
  jl dfs_floodfill_mbr\@
  cmp rsi,LEN*LEN
  jge dfs_floodfill_mbr\@
  cmp byte ptr[rdi+rsi],'@'
  je dfs_floodfill_mbr\@
  call dfs_floodfill
dfs_floodfill_mbr\@:
.endm

# (byte* ptr, uint64_t idx) -> void
dfs_floodfill:
  push rdi
  push rsi
  mov byte ptr[rdi+rsi],'@'

  DFS_FLOODFILL_MOVCHECK sub,LEN
  DFS_FLOODFILL_MOVCHECK add,LEN
  DFS_FLOODFILL_MOVCHECK sub,1
  DFS_FLOODFILL_MOVCHECK add,1

  add rsp,16
  ret

# (byte* ptr)
debug_print_map:
  push r8
  push r9
  sub rsp,8

  mov r9,rdi
  mov r8,50
loop3:
  mov rdi,r9
  mov rsi,1
  mov rdx,LEN
  mov rcx,stdout
  call aocc_fwrite
  mov rdi,'\n'
  call aocc_putchar

  add r9,LEN
  dec r8
  jnz loop3
loop3end:

  add rsp,8
  pop r9
  pop r8
  ret

.global p10b
p10b:
  push rbp
  push rbx # sum
  push r8  # ptr x
  push r9  # ptr y
  push r10
  push r11
  push r12 # saved rsp
  push r13 # i
  push r14 # supersampled map
  push r15 # obj
  sub rsp,8

  call parse_10b
  mov r15,rax

  mov rdi,LEN*LEN
  call aocc_malloc
  mov r14,rax
  mov rdi,rax
  mov rsi,' '
  mov rdx,LEN*LEN
  call aocc_memset
  
  # construct supersampling
  xor r13,r13
loop1:
  movzx rax,byte ptr[r15+r13]
  cmp rax,'\n'
  je br0end
  cmp rax,'.'
  je br0end
  cmp rax,0
  je br0end
  cmp rax,' '
  je br0end

br0a:
  # lookup mask
  mov rcx,0
loop2:
  movzx rdx,byte ptr[str1+rcx]
  cmp rdx,rax
  je loop2end

  inc rcx
  cmp rcx,7
  jl loop2
  call die
loop2end:
  mov rdi,r14
  mov rsi,r13
  mov rdx,[str0+rcx*8]
  call put_tile
br0end:

  inc r13
  cmp r13,LEN*LEN
  jl loop1
loop1end:

  # floodfill
  mov rdi,r14
  xor rsi,rsi
  call dfs_floodfill

  # accumulate
  xor rbx,rbx
  xor r9,r9
loop4:
  xor r8,r8
loop5:
  imul rax,r9,LEN
  add rax,r8
  add rax,LEN+1
  add rax,r14

  cmp byte ptr[rax-LEN-1],'@'
  je br1end
  cmp byte ptr[rax-LEN+1],'@'
  je br1end
  cmp byte ptr[rax+LEN-1],'@'
  je br1end
  cmp byte ptr[rax+LEN+1],'@'
  je br1end
br1a:
  mov byte ptr[rax],'!'
  inc rbx
br1end:

  add r8,3
  cmp r8,LEN
  jl loop5
loop5end:

  add r9,3
  cmp r9,LEN
  jl loop4
loop4end:

  mov rdi,r14
  # call debug_print_map

  mov rdi,r14
  call aocc_free
  mov rdi,r15
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
die:
  ud2