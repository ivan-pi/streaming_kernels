#!/bin/bash

set -e

if [ $# -ne 2 ]; then
  echo "Usage: $0 <test-name> <size>"
  echo "Example: $0 BS3 1000000"
  exit 1
fi

TEST_NAME="$1"
SIZE="$2"
EXE=./streaming_kernels

if [ ! -x "$EXE" ]; then
  echo "ERROR: Executable '$EXE' not found."
  exit 1
fi

if [[ "$(uname)" == "Darwin" ]]; then
  MAX_THREADS=$(sysctl -n hw.logicalcpu)
else
  MAX_THREADS=$(nproc)
fi

printf "%-8s %-15s %-15s\n" "Threads" "Time(s)" "Rate(GB/s)"

for threads in $(seq 1 "$MAX_THREADS"); do
  export OMP_NUM_THREADS=$threads
  result=$($EXE -t "$TEST_NAME" -n "$SIZE" | awk -v test="$TEST_NAME" '$1 == test { print $3, $4 }')
  # Handle possible failure (e.g. missing line)
  if [ -z "$result" ]; then
    printf "%-8d %-15s %-15s\n" "$threads" "N/A" "N/A"
  else
    time=$(echo "$result" | awk '{print $1}')
    rate=$(echo "$result" | awk '{print $2}')
    printf "%-8d %-15s %-15s\n" "$threads" "$time" "$rate"
  fi
done
