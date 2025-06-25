#!/bin/bash

# ==============================================================================
# Script: run_all.sh
# Description: Executes multiple Rscript calls to generate STN-i output files
#              for various experiments and levels. Logs output to Logs/run_all_logs.log.
#
# Usage:
#   1. Make the script executable:
#        chmod +x run_all.sh
#
#   2. Run the script:
#        ./run_all.sh
#
#   Output:
#     - A log file will be created or overwritten: Logs/run_all_logs.log
#     - STN-i output files will be generated per experiment/level in their folders.
# ==============================================================================

# Optional: Exit immediately if a command exits with a non-zero status
# set -e

# Ensure the Logs directory exists
LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"

# Define log file path
LOG_FILE="$LOG_DIR/run_all_logs.log"
echo "=== Run started at $(date) ===" > "$LOG_FILE"

# Function to execute Rscript with logging
run_rscript() {
  local args=("$@")
  local last_output_file=""

  # Extract the output file name from the arguments
  for arg in "${args[@]}"; do
    [[ $arg == --output_file=* ]] && last_output_file="${arg#--output_file=}"
  done

  echo ">> Running for output: $last_output_file" | tee -a "$LOG_FILE"

  # Execute Rscript and append output and errors to the log file
  if ! Rscript R/main.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
    echo "❌ Error: Failed to generate $last_output_file" | tee -a "$LOG_FILE"
  else
    echo "✅ Success: Generated $last_output_file" | tee -a "$LOG_FILE"
  fi
}

# Define experiments per algorithm using associative array
declare -A experiments
experiments["ACOTSP"]="E1-BL-WSR-2000 E2-BL-SR-2000 E3-BH-WSR-2000 E4-BH-SR-2000"
experiments["MMASQAP"]="E1-BL-WSR-60 E2-BL-SR-60 E3-BH-WSR-60 E4-BH-SR-60"
experiments["PSO-X"]="E1-BL-WSR-Mix E2-BL-SR-Mix E3-BH-WSR-Mix E4-BH-SR-Mix E5-BL-WSR-Mul E6-BL-SR-Mul E7-BH-WSR-Mul E8-BH-SR-Mul E9-BL-WSR-Uni E10-BL-SR-Uni E11-BH-WSR-Uni E12-BH-SR-Uni"

# Parameter levels to be applied
levels="L1 L2 L3"

# Loop over all algorithms and their respective experiments
for alg in "${!experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for exp in ${experiments[$alg]}; do
    for lvl in $levels; do
      run_rscript \
        --input="Experiments/$alg/$exp/Data" \
        --parameters="Experiments/$alg/Parameters/$lvl.csv" \
        --output="Experiments/$alg/$exp/Result" \
        --criteria=mean \
        --significance=2 \
        --output_file="STN-i-$exp-$lvl.txt"
    done
  done
done

echo "=== Run finished at $(date) ===" >> "$LOG_FILE"
