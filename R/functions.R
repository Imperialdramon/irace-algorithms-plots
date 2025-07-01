# nolint start

#' Read parameter definitions for irace tuning.
#'
#' This function reads a CSV file describing irace tuning parameters.
#' The CSV must contain the columns "NAME", "TYPE", and "VALUES_ARRAY".
#'
#' @param parameters_file `character(1)`\cr
#'   Path to the semicolon-separated parameter file.
#'
#' @return A list with one item:
#'   \describe{
#'     \item{`params`}{A `data.frame` of the parameter definitions.}
#'   }
#'
#' @export
read_parameters_file <- function(parameters_file) {
  params <- read.csv2(parameters_file, stringsAsFactors = FALSE)
  required <- c("NAME", "TYPE", "VALUES_ARRAY")
  if (!all(required %in% colnames(params))) {
    stop("The parameter file must contain columns: NAME, TYPE, and VALUES_ARRAY.")
  }
  return(list(params = params))
}

#' Check if a parameter is numeric (integer or real).
#'
#' @param param_name `character(1)`\cr
#'   Parameter name to check.
#' @param parameters_df `data.frame`\cr
#'   The parameter definitions data frame.
#'
#' @return `logical(1)`. TRUE if numeric, FALSE otherwise.
#'
#' @export
is_numeric_param <- function(param_name, parameters_df) {
  type <- parameters_df$TYPE[parameters_df$NAME == param_name]
  return(length(type) == 1 && type %in% c("i", "r"))
}

#' Generate irace configuration plots (histograms and boxplots).
#'
#' This function reads all `.Rdata` files from a folder containing irace results,
#' extracts all configurations and elite status, and generates one histogram
#' and one boxplot per parameter, comparing elite vs regular configurations.
#'
#' @param input_folder `character(1)`\cr
#'   Path to folder with irace `.Rdata` files.
#' @param parameters_file `character(1)`\cr
#'   Path to parameter definition file.
#' @param output_folder `character(1)`\cr
#'   Folder to save output plots (.png files).
#' @param escenario_name `character(1)`\cr
#'   Name of the tuning scenario, used in file naming.
#'
#' @export
generate_irace_plots <- function(input_folder, parameters_file, output_folder, escenario_name) {
  files <- list.files(input_folder, pattern = "\\.Rdata$", full.names = TRUE)
  param_list <- read_parameters_file(parameters_file)
  parameters <- param_list$params

  all_configs <- list()

  for (file in files) {
    load(file)
    if (!exists("iraceResults")) next
    elites <- unique(unlist(iraceResults$allElites))
    iterations <- iraceResults$raceData
    id_count <- 1
    for (i in seq_along(iterations)) {
      sampled_ids <- iterations[[i]]$.ID.
      for (id in sampled_ids) {
        conf <- getConfigurationById(iraceResults, id, drop.metadata = TRUE)
        conf$.ID. <- id_count
        conf$Elite <- id %in% elites
        conf$Run <- basename(file)
        all_configs <- append(all_configs, list(conf))
        id_count <- id_count + 1
      }
    }
  }

  if (length(all_configs) == 0) stop("No configurations found.")

  df <- bind_rows(all_configs) %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(Elite = as.logical(Elite))

  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  plot_histograms(df, parameters, output_folder, escenario_name)
  plot_boxplots(df, parameters, output_folder, escenario_name)
}

#' Generate parameter histograms (Elite vs Regular).
#'
#' One histogram per parameter, overlaying elite and regular distributions.
#'
#' @param df `data.frame`\cr Irace configurations including `Elite`.
#' @param parameters_df `data.frame`\cr Parameter definitions.
#' @param output_folder `character(1)`\cr Folder to save plots.
#' @param escenario_name `character(1)`\cr Scenario name for filenames.
#'
#' @export
plot_histograms <- function(df, parameters_df, output_folder, escenario_name) {
  parameter_cols <- setdiff(names(df), c(".ID.", "Elite", "Run"))

  for (param in parameter_cols) {
    is_numeric <- is_numeric_param(param, parameters_df)

    output_file <- file.path(output_folder, paste0("hist_", escenario_name, "_", param, ".png"))
    png(filename = output_file, width = 900, height = 500, res = 120)

    df_filtered <- df[!is.na(df[[param]]), ]

    if (nrow(df_filtered) == 0) {
      dev.off()
      next
    }

    if (is_numeric) {
      na_counts <- df %>%
        group_by(Elite) %>%
        summarise(na_count = sum(is.na(.data[[param]])), .groups = "drop")

      p <- ggplot(df_filtered, aes(x = .data[[param]], fill = Elite)) +
        geom_histogram(position = "identity", alpha = 0.6, bins = 30, color = "black") +
        scale_fill_manual(values = c("FALSE" = "#999999", "TRUE" = "#56B4E9"),
                          labels = c("Regular", "Elite")) +
        ggtitle(param) +
        xlab("Values") + ylab("Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.8,
                 label = paste0("NA (Elite): ", na_counts$na_count[na_counts$Elite == TRUE],
                                "\nNA (Regular): ", na_counts$na_count[na_counts$Elite == FALSE]),
                 size = 3, color = "darkred")

    } else {
      df_filtered <- df[!is.na(df[[param]]), ]

      p <- ggplot(df_filtered, aes(x = .data[[param]], fill = Elite)) +
        geom_bar(position = "dodge", alpha = 0.8, color = "black") +
        scale_fill_manual(values = c("FALSE" = "#999999", "TRUE" = "#56B4E9"),
                          labels = c("Regular", "Elite")) +
        ggtitle(param) +
        xlab("Values") + ylab("Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }

    print(p)
    dev.off()
  }
}

#' Generate compact boxplots per parameter (Elite vs Regular).
#'
#' One boxplot per parameter, comparing elite vs regular values.
#'
#' @param df `data.frame`\cr Irace configurations including `Elite`.
#' @param parameters_df `data.frame`\cr Parameter definitions.
#' @param output_folder `character(1)`\cr Folder to save plots.
#' @param escenario_name `character(1)`\cr Scenario name for filenames.
#'
#' @export
plot_boxplots <- function(df, parameters_df, output_folder, escenario_name) {
  parameter_cols <- setdiff(names(df), c(".ID.", "Elite", "Run"))

  for (param in parameter_cols) {
    if (!is_numeric_param(param, parameters_df)) next

    output_file <- file.path(output_folder, paste0("box_", escenario_name, "_", param, ".png"))
    png(filename = output_file, width = 600, height = 250, res = 120)

    df_filtered <- df[is.finite(df[[param]]), ]
    if (nrow(df_filtered) == 0) {
      dev.off()
      next
    }

    na_counts <- df %>%
      group_by(Elite) %>%
      summarise(na_count = sum(is.na(.data[[param]])), .groups = "drop")

    p <- ggplot(df_filtered, aes(x = Elite, y = .data[[param]], fill = Elite)) +
      geom_boxplot(outlier.color = "red", alpha = 0.7) +
      scale_fill_manual(values = c("FALSE" = "#999999", "TRUE" = "#56B4E9"),
                        labels = c("Regular", "Elite")) +
      ggtitle(param) +
      xlab(NULL) + ylab("Value") +
      theme_minimal(base_size = 9) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 9),
        panel.border = element_rect(color = "black", fill = NA)
      ) +
      annotate("text",
               x = 2.1, y = Inf,
               label = paste0("NA (Elite): ", na_counts$na_count[na_counts$Elite == TRUE],
                              "\nNA (Regular): ", na_counts$na_count[na_counts$Elite == FALSE]),
               hjust = 1.1, vjust = 1.5,
               size = 3, color = "darkred", fontface = "bold"
      )

    print(p)
    dev.off()
  }
}

# nolint end
