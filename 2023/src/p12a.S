# let dp, c1, c2
# dp[i][j][k] = how many arrangements can be constructed at some state
# i = first i elements
# j = first j runs
# k = the last k elements have been #
# dp[0][0][0] = 1
# >>> c1[i] == '?' || c1[i] == '#'
# forall(j,k>0) dp[i][j][k] = dp[i-1][j][k-1]
# >>> c1[i] == '?' || c1[i] == '.'
# forall(j) dp[i][j][0] = dp[i-1][j-1][c2[j-1]] + dp[i-1][j][0]
# >>> end
# answer = dp[len(c1)][len(c2)][0]

.intel_syntax noprefix

#define DPDIM 8

# struct obj { char c1[256]; uint32_t c2[64]; uint64_t c1len, uint64_t c2len }
# (file*) -> obj*
parse_single_line_12a:
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

  # add a trailing dot
  mov byte ptr[r12],'.'
  inc r12
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
  shr r12,3
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


single_line_12a:
  push rbp
  push rbx
  push r8  # i
  push r9  # j
  push r10 # k
  push r11
  push r12
  push r13
  push r14 # dp arr
  push r15 # obj
  sub rsp,8

  mov r15,rdi

  # i*dim*dim+j*dim+k
  mov rdi,DPDIM*DPDIM*DPDIM*8
  call aocc_malloc
  mov r14,rax
  mov rdi,rax
  xor rsi,rsi
  mov rdx,DPDIM*DPDIM*DPDIM*8
  call aocc_memset

  mov qword ptr[r14],1

  xor r8,r8
  inc r8
loop3i:
  xor r9,r9
loop3j:

  cmp byte ptr[r15+r8-1],'.'
  je br2b
  cmp byte ptr[r15+r8-1],'#'
  je br2a
  cmp byte ptr[r15+r8-1],'?'
  je br2a
  call die
br2a:
  xor r10,r10
  inc r10
loop3k:
  imul rdi,r8,DPDIM
  add rdi,r9
  imul rdi,rdi,DPDIM
  add rdi,r10

  lea rsi,[rdi-1-DPDIM*DPDIM]

  mov rax,[r14+rdi*8]
  mov [r14+rsi*8],rax

  inc r10
  cmp r10,[r15+512]
  jle loop3k
loop3kend:

  cmp byte ptr[r15+r8-1],'?'
  jne br2end
br2b:

  mov ecx,[r15+256+r9*4-4]

  imul rdi,r8,DPDIM
  add rdi,r9
  imul rdi,rdi,DPDIM
  
  mov rsi,rdi
  sub rsi,DPDIM*DPDIM+DPDIM
  add rsi,rcx

  mov rax,[r14+rsi*8]
  mov [r14+rdi*8],rax

  mov rsi,rdi
  sub rsi,DPDIM*DPDIM

  mov rax,[r14+rsi*8]
  add [r14+rdi*8],rax

br2end:

  inc r9
  cmp r9,[r15+520]
  jle loop3j
loop3jend:
  inc r8
  cmp r8,[r15+512]
  jle loop3i
loop3iend:
  
  imul rsi,[r15+512],DPDIM
  add rsi,[r15+520]
  imul rsi,rsi,DPDIM
  mov rax,[r15+rsi*8]

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

.global p12a
p12a:
  push rbp
  push rbx # counter
  push r8
  push r9
  push r10
  push r11
  push r12
  push r13
  push r14
  push r15 # file
  sub rsp,8

  mov r15,rdi
  xor rbx,rbx
loop2:
  mov rdi,r15
  call parse_single_line_12a
  cmp rax,0
  je loop2end

  mov rdi,rax
  call single_line_12a
  add rbx,rax

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