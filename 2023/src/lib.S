.intel_syntax noprefix

.macro aoccwrap fnname
.global aocc_\fnname
aocc_\fnname:
  push r8
  push r9
  push r10
  push r11
  sub rsp,8
  
  call \fnname

  add rsp,8
  pop r11
  pop r10
  pop r9
  pop r8
  ret
.endm

aoccwrap fgets
aoccwrap free
aoccwrap malloc
aoccwrap strtol
aoccwrap strtoll
aoccwrap fread
aoccwrap fwrite
aoccwrap memset
aoccwrap qsort
aoccwrap getc
aoccwrap printf
aoccwrap strlen
aoccwrap putchar