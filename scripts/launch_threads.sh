#!/bin/bash

# Configuration
SCRIPT="./scripts/run_threads.sh"
SIZE=61341696
MAX_T=16 # Optional: override thread count here
EXE_NAME="./streaming_kernels"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
ZIP_NAME="bench_results_${TIMESTAMP}.zip"

echo "Starting Benchmark Suite..."

# Run the 5 tests
for i in {1..5}; do
  TEST_ID="BS$i"
  OUT_FILE="result_bs$i.txt"
  
  echo "--> Executing $TEST_ID..."
  
  # Calling with the new flags
  $SCRIPT -m "$MAX_T" -e "$EXE_NAME" "$TEST_ID" "$SIZE" > "$OUT_FILE"
done

echo "All tests complete. Results saved to result_bs*.txt"

echo "---------------------------------------"
echo "Packaging results into $ZIP_NAME..."

if command -v zip >/dev/null 2>&1; then
    zip "$ZIP_NAME" result_bs*.txt
    echo "Success! Final file: $ZIP_NAME"
else
    TAR_NAME="bench_results_${TIMESTAMP}.tar.gz"
    tar -czvf "$TAR_NAME" result_bs*.txt
    echo "Success! Final file: $TAR_NAME"
fi