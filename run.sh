#!/bin/bash

set -e

ghc -package vector -package split -package memoize -package containers -o build/aoc2024 -O 2024/$1.hs
build/aoc2024