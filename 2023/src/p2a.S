.intel_syntax noprefix
#include "lib.h.S"

.global p2a
p2a:
  mov rax,90
  lib_mul rax,7
  ret