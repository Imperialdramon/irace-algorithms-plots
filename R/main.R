#!/usr/bin/env Rscript

required_packages <- c("irace", "ggplot2", "GGally", "dplyr", "MASS", "tidyr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required. Please install it."))
  }
}

suppressPackageStartupMessages({
  library(irace)
  library(ggplot2)
  library(GGally)
  library(dplyr)
  library(MASS)
  library(tidyr)
})

source("R/functions.R")

parse_arguments <- function(args) {
  parsed <- list()
  for (arg in args) {
    if (startsWith(arg, "--")) {
      kv <- strsplit(sub("^--", "", arg), "=")[[1]]
      if (length(kv) == 2) {
        parsed[[kv[1]]] <- kv[2]
      } else {
        stop(paste("Invalid argument:", arg))
      }
    }
  }
  return(parsed)
}

args <- commandArgs(trailingOnly = TRUE)
params <- parse_arguments(args)

required <- c("input", "parameters", "output", "escenario_name")
missing <- setdiff(required, names(params))
if (length(missing) > 0) stop(paste("Missing arguments:", paste(missing, collapse = ", ")))

generate_irace_plots(
  input_folder = params$input,
  parameters_file = params$parameters,
  output_folder = params$output,
  escenario_name = params$escenario_name
)

if (length(warnings()) > 0) {
  cat("\n--- WARNINGS DETECTED ---\n")
  print(warnings())
}