# let dp, c1, c2
# dp[i][j][k] = how many arrangements can be constructed at some state
# i = first i elements
# j = first j runs
# k = the last k elements have been #
# dp[0][0][0] = 1
# >>> forall(i, j) k = 0
# >> c1[i-1] == '?' || c1[i-1] == '.'
# dp[i][j][0] = dp[i-1][j-1][c2[j-1]] + dp[i-1][j][0]
# >> c1[i-1] == '#'
# dp[i][j][0] = 0
# >>> forall(i, j, k) k > 0
# >> c1[i-1] == '?' || c1[i-1] == '#'
# forall(j,k>0) dp[i][j][k] = dp[i-1][j][k-1]
# >> c1[i-1] == '.'
# forall(j,k>0) dp[i][j][k] = 0
# >>> end
# answer = dp[len(c1)][len(c2)][0]

.intel_syntax noprefix

#define DPDIM 256

str0:
  .ascii "\xff\xff\xff\xff\xff\xff\xff\xff"
str1:
  .asciz "r(%lli, %lli, %lli)\n"
str2:
  .asciz "i %lli\n"

# struct obj { char c1[256]; uint32_t c2[64]; uint64_t c1len, uint64_t c2len }
# (file*) -> obj*
parse_single_line_12b:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11
  push r12 # obj mut
  push r13 # obj
  push r14 # cline mut
  push r15 # cline
  sub rsp,8

  mov [rsp],rdi

  mov rdi,1024
  call aocc_malloc
  mov r15,rax
  mov r14,rax
  mov rdi,rax
  mov rsi,0
  mov rdx,1024
  call aocc_memset

  mov rdi,r15
  mov rsi,1024
  mov rdx,[rsp]
  call aocc_fgets
  cmp rax,0
  jne br0end
br0a:
  xor eax,eax
  jmp ret0
br0end:

  mov rdi,528
  call aocc_malloc
  mov r13,rax
  mov r12,rax
  mov rdi,rax
  xor rsi,rsi
  mov rdx,528
  call aocc_memset

loop0:
  movzx rax,byte ptr[r14]
  cmp rax,' '
  je loop0end
  cmp rax,0
  je die

  mov [r12],al
  inc r12

  inc r14
  jmp loop0
loop0end:
  sub r12,r13
  mov [r13+512],r12

  lea r12,[r13+256]
loop1:
  movzx rax,byte ptr[r14]
  cmp rax,0
  je loop1end
  cmp rax,'0'
  jl br1end
  cmp rax,'9'
  jg br1end
br1a:
  mov rdi,r14
  mov rsi,rsp
  mov rdx,10
  call aocc_strtol
  mov r14,[rsp]

  mov [r12],eax
  add r12,4
br1end:

  inc r14
  jmp loop1
loop1end:
  sub r12,r13
  sub r12,256
  shr r12,2
  mov [r13+520],r12

  mov rax,r13

ret0:
  mov [rsp],rax
  mov rdi,r15
  call aocc_free
  mov rax,[rsp]

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

unfold:
  push rbp # arg
  mov rbp,rdi

  # add a ? to c1 to join them
  mov rax,[rbp+512]
  mov byte ptr[rbp+rax],'?'
  inc qword ptr[rbp+512]

  # copy c1
.macro p12a_unfold_c1 iter
  mov rax,[rbp+512]
  imul rcx,rax,\iter
  lea rdi,[rbp+rcx]
  mov rsi,rbp
  mov rdx,rax
  call aocc_memcpy
.endm

  P12A_UNFOLD_C1 1
  P12A_UNFOLD_C1 2
  P12A_UNFOLD_C1 3
  P12A_UNFOLD_C1 4

  # copy c2
.macro p12a_unfold_c2 iter
  mov rax,[rbp+520]
  shl rax,2
  imul rcx,rax,\iter
  lea rdi,[rbp+256+rcx]
  lea rsi,[rbp+256]
  mov rdx,rax
  call aocc_memcpy
.endm

  P12A_UNFOLD_C2 1
  P12A_UNFOLD_C2 2
  P12A_UNFOLD_C2 3
  P12A_UNFOLD_C2 4

  # update lengths
  imul rax,[rbp+512],5
  mov [rbp+512],rax
  imul rax,[rbp+520],5
  mov [rbp+520],rax

  # change the final ? to a trailing .
  mov rax,[rbp+512]
  mov byte ptr[rbp+rax-1],'.'

  pop rbp
  ret

# WARN: nonstd colling conv!
# required align: 16
# r15: input, save
# r14: memo, save
# r8: i, clobber
# r9: j, clobber
# r10: k, clobber
# rax: return, clobber
# everything else: saved
.macro idx3 rd
  imul \rd,r8,DPDIM
  add \rd,r9
  imul \rd,\rd,DPDIM
  add \rd,r10
.endm
recur_12b:
  # reject negative inputs first
  cmp r8,0
  jl br5a
  cmp r9,0
  jl br5a
  cmp r10,0
  jl br5a
  jmp br5end
br5a:
  xor eax,eax
  ret
br5end:

  # lookup cache
  IDX3 rax
  mov rax,[r14+rax*8]
  cmp rax,[str0]
  je br2end
br2a:
  ret
br2end:

  # debug print invocations
  push rdi
  push rsi
  push rdx
  push rcx
  sub rsp,8
  lea rdi,[str1]
  mov rsi,r8
  mov rdx,r9
  mov rcx,r10
  # call aocc_printf
  add rsp,8
  pop rcx
  pop rdx
  pop rsi
  pop rdi

  sub rsp,8
  push rbx
  push r8
  push r9
  push r10
  
  # base cases
  cmp r8,0
  jne br4end
br4a:
  cmp r9,0
  jne br3c
  cmp r10,0
  jne br3c
  mov rax,1
  jmp br3end
br4end:

  movzx rax,byte ptr[r15+r8-1]
  cmp r10,0
  jne br3b
br3a: # k = 0
  cmp al,'#'
  je br3c
  
  # dp[i-1][j-1][c2[j-1]] + dp[i-1][j][0]
  dec r8
  dec r9
  mov r10d,[r15+256+r9*4]
  call recur_12b
  mov rbx,rax

  mov r8,[rsp+16]
  dec r8
  mov r9,[rsp+8]
  xor r10,r10
  call recur_12b
  add rax,rbx

  jmp br3end
br3b: # k > 0
  cmp al,'.'
  je br3c

  # dp[i-1][j][k-1]
  dec r8
  dec r10
  call recur_12b

  jmp br3end
br3c: # branch to jump to if should = 0
  xor eax,eax
br3end:
  pop r10
  pop r9
  pop r8
  # save to memo
  idx3 rbx
  mov [r14+rbx*8],rax
  pop rbx
  add rsp,8
  ret

# (input*, memo*)
single_line_12b:
  push rbp
  push rbx # answer
  push r8  # i
  push r9  # j
  push r10 # k
  push r11
  push r12
  push r13
  push r14 # memo
  push r15 # obj
  sub rsp,8

  mov r15,rdi
  mov r14,rsi
  mov rdi,r14
  mov rsi,0xff
  mov rdx,DPDIM*DPDIM*DPDIM*8
  call aocc_memset

  mov r8,[r15+512]
  mov r9,[r15+520]
  xor r10,r10
  call recur_12b

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

.global p12b
p12b:
  push rbp
  push rbx # sum
  push r8
  push r9
  push r10
  push r11
  push r12 # memoization array
  push r13 # counter
  push r14 # input
  push r15 # file
  sub rsp,8

  mov r15,rdi

  mov rdi,DPDIM*DPDIM*DPDIM*8
  call aocc_malloc
  mov r12,rax

  xor rbx,rbx
  xor r13,r13
loop2:
  mov rdi,r15
  call parse_single_line_12b
  cmp rax,0
  je loop2end
  mov r14,rax

  mov rdi,r14
  call unfold

  mov rdi,r14
  mov rsi,r12
  call single_line_12b
  add rbx,rax

  mov rdi,r14
  call aocc_free

  jmp loop2
loop2end:

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