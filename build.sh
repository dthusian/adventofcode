#!/bin/bash

mkdir -p build/
cd build/
cmake ../2023/ -G Ninja
cmake --build . --target adventofcode
