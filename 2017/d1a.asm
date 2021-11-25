  bits 64
section .text
  global _start
  extern a8_io_init
  extern a8_io_read_until
  extern a8_io_debug
  extern a8_exit

_start:
  