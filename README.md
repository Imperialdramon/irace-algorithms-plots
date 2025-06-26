# Analysis of Algorithm Results

This repository contains R scripts for processing and analyzing algorithm experiment results. The analysis is based on [iraceplot](https://auto-optimization.github.io/iraceplot/).

## Structure

- `R/main.R`: Main script to run the analysis.
- `R/functions.R`: Helper functions used by `main.R`.

## Usage

Run the following command to analyze your experiment results:

```sh
Rscript R/main.R \
    --input=Experiments/ACOTSP/E1-BL-WSR-2000/Data \
    --parameters=Experiments/ACOTSP/parameters.csv \
    --output=Experiments/ACOTSP/E1-BL-WSR-2000/Result \
    --escenario_name=E1-BL-WSR-2000
```

### Parameters

- `--input`: Path to the directory containing experiment data.
- `--parameters`: CSV file with parameter configuration.
- `--output`: Directory where results and plots will be saved.
- `--escenario_name`: Identifier name for the scenario or experiment.

Ensure that the scripts are in the `R/` directory and that all paths exist before running the command.