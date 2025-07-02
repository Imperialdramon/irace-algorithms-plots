# nolint start

#########################################################################
# IRACE Algorithm Plots Generator Script
# Author: [Your Name]
#
# Description:
# This script generates plots from the output of irace executions.
# It uses:
#   - Execution result files from irace runs (input folder)
#   - Parameter configuration file
#
# The script produces a set of plots for algorithm analysis and stores
# them in the specified output folder.
#
# Usage:
# Rscript main.R --input=<irace_output_folder>
#                --parameters=<parameters_file>
#                --output=<output_folder>
#                --escenario_name=<name>
#
# Arguments:
# --input          : (Required) Folder path containing irace execution outputs.
# --parameters     : (Required) Path to the parameters file used by irace.
# --output         : (Required) Path to the output folder for the generated plots.
# --global_folder  : (Optional) Path to a global folder for metrics (default: current directory).
# --escenario_name : (Required) Name of the scenario for labeling outputs.
#
# Requirements:
# - R with the following packages: irace, ggplot2, GGally, dplyr, MASS, tidyr, reshape2.
# - Auxiliary functions must be available in "R/functions.R".
#
# Output:
# - Plots and analysis files stored in the output folder.
#########################################################################

# ---------- Load required packages ----------
required_packages <- c("irace", "ggplot2", "dplyr", "tidyr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required. Please install it."), call. = FALSE)
  }
}

suppressPackageStartupMessages({
  library(irace)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

# ---------- Load utility functions ----------
source("R/functions.R")

# ---------- Parse command line arguments ----------
parse_arguments <- function(args) {
  parsed <- list()
  for (arg in args) {
    if (grepl("^--", arg)) {
      parts <- strsplit(sub("^--", "", arg), "=")[[1]]
      if (length(parts) == 2) {
        parsed[[parts[1]]] <- parts[2]
      } else {
        stop(paste("Invalid argument format:", arg), call. = FALSE)
      }
    }
  }
  return(parsed)
}

args <- commandArgs(trailingOnly = TRUE)
params <- parse_arguments(args)

# ---------- Validate required arguments ----------
required_args <- c("input", "parameters", "output", "escenario_name")

for (param_name in required_args) {
  if (is.null(params[[param_name]])) {
    stop(paste("Missing required argument: --", param_name, sep = ""), call. = FALSE)
  }
}

# ---------- Assign and normalize paths ----------
input_folder <- normalizePath(params$input, mustWork = TRUE)
parameters_file <- normalizePath(params$parameters, mustWork = TRUE)
output_folder <- normalizePath(params$output, mustWork = FALSE)
global_folder <- ifelse(is.null(params$global_folder), getwd(), normalizePath(params$global_folder, mustWork = FALSE))
escenario_name <- params$escenario_name

# Create output folder if it does not exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
  message("Output folder created: ", output_folder)
}

# ---------- Generate plots ----------
generate_irace_plots(
  input_folder = input_folder,
  parameters_file = parameters_file,
  output_folder = output_folder,
  escenario_name = escenario_name,
  global_folder = global_folder
)

# ---------- Print warnings if any ----------
if (length(warnings()) > 0) {
  cat("\n--- WARNINGS DETECTED ---\n")
  print(warnings())
}

# nolint end