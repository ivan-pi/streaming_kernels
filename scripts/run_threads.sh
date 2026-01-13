#!/bin/bash

# Default values
EXE="./streaming_kernels"
USER_MAX_THREADS=""
SIZE=61341696

usage() {
  echo "Usage: $0 [-m max_threads] [-e executable] <test-name> <size>"
  exit 1
}

# Parse optional flags
while getopts "m:e:" opt; do
  case $opt in
    m) USER_MAX_THREADS=$OPTARG ;;
    e) EXE=$OPTARG ;;
    *) usage ;;
  esac
done
shift $((OPTIND -1))

# Check required positional arguments
if [ $# -ne 2 ]; then
  usage
fi

TEST_NAME="$1"
SIZE="$2"

if [ ! -x "$EXE" ]; then
  echo "ERROR: Executable '$EXE' not found."
  exit 1
fi

# Detect Hardware Threads
if [[ "$(uname)" == "Darwin" ]]; then
  HW_THREADS=$(sysctl -n hw.logicalcpu)
else
  HW_THREADS=$(nproc)
fi

# Determine final MAX_THREADS
if [ -n "$USER_MAX_THREADS" ] && [ "$USER_MAX_THREADS" -lt "$HW_THREADS" ]; then
  MAX_THREADS=$USER_MAX_THREADS
else
  MAX_THREADS=$HW_THREADS
fi

# --- Thread Pinning Configuration ---
export OMP_PLACES=cores
export OMP_PROC_BIND=spread

printf "Running %s (Max Threads: %d)\n" "$TEST_NAME" "$MAX_THREADS"
printf "%-8s %-15s %-15s\n" "Threads" "Time(s)" "Rate(GB/s)"

for threads in $(seq 1 "$MAX_THREADS"); do
  export OMP_NUM_THREADS=$threads
  result=$($EXE -t "$TEST_NAME" -n "$SIZE" | awk -v test="$TEST_NAME" '$1 == test { print $3, $4 }')

  if [ -z "$result" ]; then
    printf "%-8d %-15s %-15s\n" "$threads" "N/A" "N/A"
  else
    read -r time rate <<< "$result"
    printf "%-8d %-15s %-15s\n" "$threads" "$time" "$rate"
  fi
done