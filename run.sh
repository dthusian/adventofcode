#!/bin/bash

set -e

ghc -o build/aoc2024 -O 2024/$1.hs
build/aoc2024