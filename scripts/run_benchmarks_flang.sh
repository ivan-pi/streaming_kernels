#!/bin/bash

set -e

EXE=./streaming_kernels
TEST_FLAG="-t BS3"

# Configuration flags for flang
config_names=("flang_default" "flang_fast" "flang_unroll")
config_flags=(
  "-O3 -mcpu=native"
  "-O3 -mcpu=native -ffast-math"
  "-O3 -mcpu=native -ffast-math -funroll-loops"
)

run_benchmark() {
  label="$1"
  echo "==> Running benchmark: $label"
  $EXE $TEST_FLAG | grep 'BS3$'
}

# Loop over configurations
for i in 0 1 2; do
  name="${config_names[$i]}"
  flags="${config_flags[$i]}"
  echo "==> Building: $name"
  echo "FC = flang"
  echo "FFLAGS = $flags"
  make clean >/dev/null 2>&1
  make streaming_kernels FC=flang FFLAGS="$flags" >/dev/null
  run_benchmark "$name"
done
