#!/bin/bash

# ==============================================================================
# Script: run_all.sh
# Description: Executes multiple Rscript calls to generate irace plots
#              for all experiments in the Experiments folder. 
#              Logs output to Logs/run_all_logs.log.
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
#     - Plot files will be generated per experiment in their Result folders.
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
  local escenario_name=""

  # Extract the escenario name from the arguments
  for arg in "${args[@]}"; do
    [[ $arg == --escenario_name=* ]] && escenario_name="${arg#--escenario_name=}"
  done

  echo ">> Running for escenario: $escenario_name" | tee -a "$LOG_FILE"

  # Execute Rscript and append output and errors to the log file
  if ! Rscript R/main.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
    echo "❌ Error: Failed to generate plots for $escenario_name" | tee -a "$LOG_FILE"
  else
    echo "✅ Success: Generated plots for $escenario_name" | tee -a "$LOG_FILE"
  fi
}

# Define experiments per algorithm using associative array
declare -A experiments
experiments["ACOTSP"]="E1-BL-WSR-2000 E2-BL-SR-2000 E3-BH-WSR-2000 E4-BH-SR-2000"
experiments["MMASQAP"]="E1-BL-WSR-60 E2-BL-SR-60 E3-BH-WSR-60 E4-BH-SR-60"
experiments["PSO-X"]="E1-BL-WSR-Mix E2-BL-SR-Mix E3-BH-WSR-Mix E4-BH-SR-Mix E5-BL-WSR-Mul E6-BL-SR-Mul E7-BH-WSR-Mul E8-BH-SR-Mul E9-BL-WSR-Uni E10-BL-SR-Uni E11-BH-WSR-Uni E12-BH-SR-Uni"

# Loop over all algorithms and their respective experiments
for alg in "${!experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for exp in ${experiments[$alg]}; do
    # Check if the experiment directory exists
    exp_dir="Experiments/$alg/$exp"
    if [ ! -d "$exp_dir" ]; then
      echo "⚠️  Warning: Directory $exp_dir does not exist, skipping..." | tee -a "$LOG_FILE"
      continue
    fi

    # Check if Data directory exists
    data_dir="$exp_dir/Data"
    if [ ! -d "$data_dir" ]; then
      echo "⚠️  Warning: Data directory $data_dir does not exist, skipping..." | tee -a "$LOG_FILE"
      continue
    fi

    # Check if parameters file exists
    params_file="Experiments/$alg/parameters.csv"
    if [ ! -f "$params_file" ]; then
      echo "⚠️  Warning: Parameters file $params_file does not exist, skipping..." | tee -a "$LOG_FILE"
      continue
    fi

    # Ensure Result directory exists
    result_dir="$exp_dir/Result"
    mkdir -p "$result_dir"

    # Run the Rscript for this experiment
    run_rscript \
      --input="$data_dir" \
      --parameters="$params_file" \
      --output="$result_dir" \
      --escenario_name="$exp" \
      --global_folder="Resumen"
  done
done

echo "=== Run finished at $(date) ===" >> "$LOG_FILE"
echo "🎉 All experiments processed! Check the log file: $LOG_FILE"
