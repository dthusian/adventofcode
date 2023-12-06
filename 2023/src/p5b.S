.intel_syntax noprefix
#include "lib.h.S"

# format: struct { ptrdiff_t offsets[8]; uint64_t data[]; }

# __aocc (FILE*) -> void*
parse_5b:
  push rbp # struct buf
  push rbx # struct buf write ptr
  push r8
  push r9
  push r10
  push r11
  push r12 # array id
  push r13 # FILE* buf mut
  push r14 # FILE* buf
  push r15 # FILE*
  sub rsp,8 # strtol out

  mov r15,rdi
  
  mov rdi,1024*128
  call aocc_malloc
  mov r14,rax
  mov rdi,rax
  mov rsi,0
  mov rdx,1024*128
  call aocc_memset

  mov rdi,r14
  mov rsi,1024*128
  mov rdx,1
  mov rcx,r15
  call aocc_fread
  mov r13,r14

  xor r12,r12

  mov rdi,1024*128
  call aocc_malloc
  mov rbp,rax
  lea rbx,[rbp+64]

loop0: # skip first :
  movzx rax,byte ptr[r13]
  inc r13
  cmp rax,':'
  jne loop0
loop0end:

loop1:
  movzx rax,byte ptr[r13]
  # check nul
  cmp rax,0
  je loop1end

  # check colon
  cmp rax,':'
  jne br0end
br0a: 
  mov [rbp+r12*8],rbx
  inc r12
  inc r13
  jmp loop1
br0end:

  # check num
  cmp rax,'0'
  jl br1end
  cmp rax,'9'
  jg br1end
br1a:
  mov rdi,r13
  mov rsi,rsp
  mov rdx,10
  call aocc_strtol
  mov r13,[rsp]
  mov [rbx],rax
  add rbx,8
br1end:

  inc r13
  jmp loop1
loop1end:
  mov [rbp+r12*8],rbx

  mov rdi,r14
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

# struct transform_info {
#   uint64_t* fnbegin,
#   uint64_t* fnend,
#   uint64_t* out_cq_r,
#   uint64_t* out_cq_w,
#   uint64_t* out_nq_r,
#   uint64_t* out_nq_w
# }
# (transform_info* info) -> void
transform_ranges:
  push r8 # rangecheck A low
  push r9 # rangecheck A hi
  push r10 # rangecheck B low
  push r11 # rangecheck A hi
  push r12
  push r13
  push rbx # scratch
  # rax: short term scratch
  # rcx,rdx: range being checked
  # rdi: transform_info
  # rsi: filter ptr
  
  mov rsi,[rdi]
  # get from cq_r
  mov rax,[rdi+16]
  mov rcx,[rax]
  mov rdx,[rax+8]
  add qword ptr[rdi+16],16
  # basic check for 0 size
  cmp rdx,0
  je loop2end2
loop2:
  # check range intersection
  mov r8,rcx
  mov r9,rdx
  add r9,r8

  mov r10,[rsi+8]
  mov r11,[rsi+16]
  add r11,r10
  mov r12,r10
  mov r13,r11
  # make the intersection
  cmp r10,r8
  cmovb r10,r8
  cmp r11,r9
  cmova r11,r9
  # r8,9: old range
  # r10,11: new range
  # r12,r13: old filter
  # check non-intersection (since they're half-open, check >= is enough)
  cmp r10,r11
  jae br2end
br2a:
  # find the non-intersecting portions of the range
  # . if residual range exists on top
  cmp r13,r9
  jae br3end
br3a:
  # r13,r9: conv back to start-length
  mov rbx,r9
  sub rbx,r13
  # add to cq_w
  mov rax,[rdi+24]
  mov [rax],r13
  mov [rax+8],rbx
  add qword ptr[rdi+24],16
br3end:
  # . if residual range exists on bottom
  cmp r8,r12
  jae br4end
br4a:
  # r8,r12: conv back to start-length
  mov rbx,r12
  sub rbx,r8
  # add to cq_w
  mov rax,[rdi+24]
  mov [rax],r8
  mov [rax+8],rbx
  add qword ptr[rdi+24],16
br4end:
  # remap both coordinates
  sub r10,[rsi+8]
  sub r11,[rsi+8]
  add r10,[rsi]
  add r11,[rsi]
  # convert back to start-len format
  sub r11,r10
  # add to nq_w
  mov rax,[rdi+40]
  mov [rax],r10
  mov [rax+8],r11
  add qword ptr[rdi+40],16
  jmp loop2end2
br2end:

  add rsi,24
  cmp rsi,[rdi+8]
  jb loop2
loop2end1:
  # didnt match any filters, add to nq_w
  mov rax,[rdi+40]
  mov [rax],rcx
  mov [rax+8],rdx
  add qword ptr[rdi+40],16

loop2end2:
  pop rbx
  pop r13
  pop r12
  pop r11
  pop r10
  pop r9
  pop r8
  ret

#define QUEUE_SIZE (1024*256)

.global p5b
p5b:
  push rbp
  push rbx
  push r8
  push r9
  push r10
  push r11 # min
  push r12 # nq
  push r13 # cq
  push r14 # loop3 counter ptr (offsets array)
  push r15 # parsed struct
  sub rsp,40 # transform_info

  call parse_5b
  mov r15,rax

  # allocate cq
  mov rdi,QUEUE_SIZE
  call aocc_malloc
  test rax,rax
  jz die
  mov [rsp+32],rax
  mov [rsp+40],rax
  mov r12,rax
  xor r13,r13
  # copy elems into cq
  lea rcx,[r15+64]
loop6:
  mov rax,[rsp+40]
  movdqu xmm0,[rcx]
  movdqu [rax],xmm0

  add qword ptr[rsp+40],16
  add rcx,16
  cmp rcx,[r15]
  jb loop6
loop6end:

  mov r14,r15
loop3:
  # initialize transform_info a bit
  mov rax,[r14]
  mov [rsp],rax
  mov rax,[r14+8]
  mov [rsp+8],rax
  # move nq->cq
  mov rdi,r13
  call aocc_free
  mov rax,[rsp+32]
  mov [rsp+16],rax
  mov rax,[rsp+40]
  mov [rsp+24],rax
  mov r13,r12
  # allocate nq
  mov rdi,QUEUE_SIZE
  call aocc_malloc
  test rax,rax
  jz die
  mov [rsp+32],rax
  mov [rsp+40],rax
  mov r12,rax
  # loop till cq is empty
loop4:
  mov rdi,rsp
  call transform_ranges

  mov rax,[rsp+16]
  mov rcx,[rsp+24]
  cmp rax,rcx
  jb loop4
loop4end:

  add r14,8
  lea rax,[r15+56]
  cmp r14,rax
  jb loop3
loop3end:

  xor r11,r11
  dec r11
loop5:
  mov rax,[rsp+32]
  mov rdi,[rax]
  add qword ptr[rsp+32],16

  cmp r11,rdi
  cmova r11,rdi

  mov rax,[rsp+32]
  mov rcx,[rsp+40]
  cmp rax,rcx
  jb loop5
loop5end:

  mov rdi,r12
  call aocc_free

  mov rdi,r13
  call aocc_free

  mov rdi,r15
  call aocc_free

  mov rax,r11

  add rsp,40
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