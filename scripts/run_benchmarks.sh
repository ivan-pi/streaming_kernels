#!/bin/bash

set -e

EXE="./streaming_kernels -n 10000000"
TEST_FLAG="-t BS3"

# Names and corresponding FFLAGS
config_names=("default" "fast" "unroll" "unroll_and_expand")
config_flags=(
  "-O3 -mcpu=native"
  "-O3 -mcpu=native -ffast-math"
  "-O3 -mcpu=native -ffast-math -funroll-loops"
  "-O3 -mcpu=native -ffast-math -funroll-loops -fvariable-expansion-in-unroller"
)

expand_values="2 3 4 5 6"

run_benchmark() {
  label="$1"
  echo "==> Running benchmark: $label"
  $EXE $TEST_FLAG | grep 'BS3$'
}

# Loop over simple configs
for i in 0 1 2 3; do
  name="${config_names[$i]}"
  flags="${config_flags[$i]}"
  echo "==> Building: $name"
  echo "FFLAGS = $flags"
  make clean >/dev/null 2>&1
  make streaming_kernels FFLAGS="$flags" >/dev/null
  run_benchmark "$name"
done

# Loop over unroll_and_expand with varying param values
base_flags="${config_flags[3]}"
for val in $expand_values; do
  name="unroll_and_expand_${val}"
  flags="$base_flags --param max-variable-expansions-in-unroller=$val"
  echo "==> Building: $name"
  echo "FFLAGS = $flags"
  make clean >/dev/null 2>&1
  make streaming_kernels FFLAGS="$flags" >/dev/null
  run_benchmark "$name"
done
