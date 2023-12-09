.intel_syntax noprefix

#define INPUTSZ 1000

str0:
  .quad 0xffffffffff
str1:
  .ascii "\x01\x01\x01\x01\x01\x00\x00\x00"
  .ascii "\x02\x01\x01\x01\x00\x00\x00\x00"
  .ascii "\x02\x02\x01\x00\x00\x00\x00\x00"
  .ascii "\x03\x01\x01\x00\x00\x00\x00\x00"
  .ascii "\x03\x02\x00\x00\x00\x00\x00\x00"
  .ascii "\x04\x01\x00\x00\x00\x00\x00\x00"
  .ascii "\x05\x00\x00\x00\x00\x00\x00\x00"
str2:
  .ascii "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09"
  .ascii "\x00\x00\x00\x00\x00\x00\x00"
  .ascii "\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x00\x00\x12\x00\x00\x10\x00\x00\x00\x00\x00\x00"
str3:
  .ascii "JJJJJ\x00\x00\x00"

# format: struct { char cards[5]; char hist_k[5]; char hist_d[5]; char type; uint32_t bet; }
# size: 20
# type ids: high card = 0, one pair = 1, two pair = 2, three of a kind = 3,
#   full house = 4, four of a kind = 5, five of a kind = 6

parse_7a:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11 # objs mut
  push r12 # objs
  push r13 # line mut
  push r14 # line
  push r15 # file
  sub rsp,8

  mov r15,rdi
  mov rdi,1024
  call aocc_malloc
  mov r14,rax
  mov r13,rax

  mov rdi,65536
  call aocc_malloc
  mov r12,rax
  mov r11,rax
  mov rdi,rax
  xor rsi,rsi
  mov rdx,65536
  call aocc_memset

loop0:
  mov rdi,r14
  mov rsi,1024
  mov rdx,r15
  call aocc_fgets
  mov r13,r14
  cmp rax,0
  je loop0end

  mov rax,[r14]
  and rax,[str0]
  mov [r11],rax

  lea rdi,[r14+6]
  xor rsi,rsi
  mov rdx,10
  call aocc_strtol
  mov [r11+16],eax

  add r11,20
  jmp loop0
loop0end:

  mov rdi,r14
  call aocc_free

  mov rax,r12

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

make_hist:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11
  push r12
  push r13
  push r14
  push r15
  sub rsp,8

  # rdi: obj mut
  # rdx: current card
  # rax: short scratch
  mov rcx,INPUTSZ # loop counter ->0
loop1:
  mov rdx,[rdi]
  and rdx,[str0]
  
  # inner loop: loop through each card
  mov r8,5 # loop counter ->0
loop2:
  # dl contains current card

  # inner inner loop: match each hist bucket or create new one
  xor r9,r9 # loop counter 0->5
loop3:
  movzx eax,byte ptr[rdi+5+r9]
  cmp al,dl
  je br0a
  cmp al,0
  je br0a
  jmp br0end
br0a:
  mov byte ptr[rdi+5+r9],dl
  inc byte ptr[rdi+10+r9]
  jmp loop3end
br0end:

  inc r9
  cmp r9,5
  jl loop3
loop3end:

  shr rdx,8 # next card
  dec r8
  jnz loop2
loop2end:

  mov r8,255
  mov r9,255
  # make the joker substitution before sorting
  # r8: joker bucket
  # r9: biggest bucket
  # rbp: biggest bucket count

  xor rsi,rsi # counter
loop9:
  movzx rax,byte ptr[rdi+5+rsi]
  cmp rax,'J'
  cmove r8,rsi

  inc rsi
  cmp rsi,5
  jl loop9
loop9end:

  mov rbp,0
  xor rsi,rsi
loop8:
  cmp rsi,r8 # the jokers cannot be selected as a target for joker-sub
  je br5end
br5a:
  movzx rax,byte ptr[rdi+10+rsi]
  cmp rbp,rax
  cmovl rbp,rax
  cmovl r9,rsi
br5end:

  inc rsi
  cmp rsi,5
  jl loop8
loop8end:

  # . if r8 or r9 were never assigned, don't do sub
  cmp r8,255
  je br6end
  cmp r9,255
  je br6end
br6a:
  # joker sub
  movzx rax,byte ptr[rdi+10+r8]
  add byte ptr[rdi+10+r9],al
  mov byte ptr[rdi+10+r8],0
br6end:

  # rdi: buf
  # rcx: loopcnt

  # at this point, hist buckets are populated
  # sort hist buckets using bubble sort
  # n = 5 so it doesn't matter
  # matters more it's easy to impl in assembly
  xor rsi,rsi # ptr to which card to check
loop4:
  movzx eax,byte ptr[rdi+10+rsi]
  movzx ebx,byte ptr[rdi+11+rsi]
  cmp al,bl
  jge br1end
br1a:
  # swap both k, d
  mov [rdi+11+rsi],al
  mov [rdi+10+rsi],bl
  movzx eax,byte ptr[rdi+5+rsi]
  movzx ebx,byte ptr[rdi+6+rsi]
  mov [rdi+6+rsi],al
  mov [rdi+5+rsi],bl
  # reset ptr
  xor rsi,rsi
  jmp loop4
br1end:

  inc rsi
  cmp rsi,4
  jl loop4
loop4end:

  # at this point, hist buckets are populated and sorted
  # now can classify type
  mov rax,[rdi+10]
  and rax,[str0]
  mov rsi,6 # loop counter
loop5:
  cmp rax,[str1+rsi*8]
  je loop5end
  dec rsi
  jnz loop5
loop5end:
  mov [rdi+15],sil

  add rdi,20
  dec rcx
  jnz loop1
loop1end:

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

qsort_cmp:
  push rbx
  # compare types first
  movzx eax,byte ptr[rdi+15]
  movzx ebx,byte ptr[rsi+15]
  cmp eax,ebx
  je br2end
br2a:
  sub eax,ebx
  pop rbx
  ret
br2end:
  # compare cards
  mov rax,[rdi]
  mov rbx,[rsi]
  and rax,[str0]
  and rbx,[str0]
  # check eq first
  cmp rax,rbx
  jne br3end
br3a:
  xor eax,eax
  pop rbx
  ret
br3end:

  # loop
loop7:
  movzx r8,al
  movzx r9,bl
  sub r8,48
  sub r9,48
  movzx r8,byte ptr[str2+r8]
  movzx r9,byte ptr[str2+r9]
  cmp r8,r9
  je br4end
br4a:
  sub r8,r9
  mov rax,r8
  pop rbx
  ret
br4end:
  shr rax,8
  shr rbx,8
  jmp loop7
loop7end:
  ud2

.global p7b
p7b:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11
  push r12
  push r13
  push r14 # sum
  push r15 # obj
  sub rsp,8

  call parse_7a
  mov r15,rax
  mov rdi,rax
  call make_hist

  # sort the darn thing
  mov rdi,r15
  mov rsi,INPUTSZ
  mov rdx,20
  lea rcx,[qsort_cmp]
  call aocc_qsort

  # sum the bets
  xor r14,r14 # sum
  xor rcx,rcx # loop count 0->INPUTSZ
loop6:
  mov rax,rcx
  imul rax,rax,20
  add rax,16
  mov eax,[r15+rax]
  lea rdx,[rcx+1]
  imul rax,rdx
  add r14,rax

  inc rcx
  cmp rcx,INPUTSZ
  jl loop6
loop6end:

  mov rdi,r15
  call aocc_free

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