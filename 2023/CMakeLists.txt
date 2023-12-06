cmake_minimum_required(VERSION 3.10)

project(adventofcode ASM)

add_executable(adventofcode
        src/main.S
        src/lib.S
        src/p1a.S
        src/p1b.S
        src/p2a.S
        src/p2b.S
        src/p3a.S
        src/p3b.S
        src/p4a.S
        src/p4b.S
        src/p5a.S
        src/p5b.S
        src/p6a.S
        src/p6b.S
        )
target_include_directories(adventofcode PRIVATE src/)