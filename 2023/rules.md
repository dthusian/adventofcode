# AOC in x86_64 Assembly Challenge

Rules:
- No input preprocessing is allowed, with one exception. Otherwise, solutions must take the input file verbatim.
  - Exception: The solution can demand that trailing whitespace either be added or omitted.
- No output postprocessing is allowed, with one exception. Otherwise, solutions must output verbatim the string that is to be submitted.
  - Exception: If the string is encoded in ASCII art, the solution can output the art instead.
    An example of this is AOC 2021 Day 13 part 2.
- All solutions must run in under 10 seconds, as measured by the `user` time in the `time` shell built-in.
- The solution must be written entirely in x86_64 assembly language, with 1 `CMakeLists.txt` file allowed to setup the build system.
  - Any assembler may be used, I used GNU as.
- No external libraries may be used, except `libc`.