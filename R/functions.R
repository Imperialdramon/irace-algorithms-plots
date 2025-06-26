# nolint start

#' Reads a parameters definition file for irace tuning.
#'
#' This function reads a CSV file containing parameter definitions for an irace tuning scenario.
#' It expects the file to include at least the columns "NAME", "TYPE", and "VALUES_ARRAY".
#' The function validates the presence of these columns and returns the parameters as a data frame.
#'
#' @param parameters_file `character(1)`\cr
#'   Path to the parameters CSV file (semicolon-separated).
#'
#' @return A list with one element:
#'   \describe{
#'     \item{`params`}{A `data.frame` containing the processed parameters.}
#'   }
#'
#' @examples
#' \dontrun{
#'   param_list <- read_parameters_file("parameters.csv")
#'   params <- param_list$params
#' }
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

#' Checks if a parameter is numeric (integer or real).
#'
#' This helper function determines whether a given parameter, identified by name,
#' is of numeric type ("i" for integer or "r" for real) based on the parameters data frame.
#'
#' @param param_name `character(1)`\cr
#'   Name of the parameter to check.
#' @param parameters_df `data.frame`\cr
#'   Data frame containing parameter definitions, including columns "NAME" and "TYPE".
#'
#' @return `logical(1)`. Returns `TRUE` if the parameter is numeric, `FALSE` otherwise.
#'
#' @export
is_numeric_param <- function(param_name, parameters_df) {
  type <- parameters_df$TYPE[parameters_df$NAME == param_name]
  return(length(type) == 1 && type %in% c("i", "r"))
}

#' Main function to generate irace configuration plots.
#'
#' This function processes irace result files from a given input folder, extracts configuration data,
#' and generates frequency histograms for all configurations and for elite configurations.
#' The plots are saved as PDF files in the specified output folder.
#'
#' @param input_folder `character(1)`\cr
#'   Path to the folder containing irace `.Rdata` result files.
#' @param parameters_file `character(1)`\cr
#'   Path to the parameters CSV file.
#' @param output_folder `character(1)`\cr
#'   Path to the folder where output plots will be saved.
#' @param escenario_name `character(1)`\cr
#'   Name of the scenario, used in output file naming.
#'
#' @return No return value. Generates and saves plots as side effects.
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

  # Frequency plots
  plot_histograms(df, parameters, output_folder, escenario_name, "all")

  df_elite <- df %>% filter(Elite)
  if (nrow(df_elite) > 0) {
    #plot_parallel_coordinates_irace_style(df_elite, parameters, output_folder, escenario_name)
    plot_histograms(df_elite, parameters, output_folder, escenario_name, "elites")
  }
}

#' Plots frequency histograms for irace configurations.
#'
#' This function generates and saves histograms for each parameter in the provided data frame,
#' distinguishing between numeric and categorical parameters. For numeric parameters, it overlays
#' a scaled density curve on the histogram. The resulting plots are saved as a single PDF file.
#'
#' @param df `data.frame`\cr
#'   Data frame containing irace configurations.
#' @param parameters_df `data.frame`\cr
#'   Data frame with parameter definitions.
#' @param output_folder `character(1)`\cr
#'   Path to the folder where the PDF will be saved.
#' @param escenario_name `character(1)`\cr
#'   Name of the scenario, used in output file naming.
#' @param tag `character(1)`\cr
#'   Tag to distinguish between all configurations and elite configurations in the output file name.
#'
#' @return No return value. Generates and saves a PDF file as a side effect.
#'
#' @export
plot_histograms <- function(df, parameters_df, output_folder, escenario_name, tag) {
  pdf(file.path(output_folder, paste0("freq_", tag, "_", escenario_name, ".pdf")), width = 7, height = 4)
  parameter_cols <- setdiff(names(df), c(".ID.", "Elite", "Run"))

  for (param in parameter_cols) {
    is_numeric <- is_numeric_param(param, parameters_df)

    if (is_numeric) {
      # Count NA values for annotation
      na_count <- sum(is.na(df[[param]]))
      # Filter out non-finite values to avoid warnings
      df_filtered <- df[is.finite(df[[param]]), ]
      
      if (nrow(df_filtered) == 0) {
        # Skip this parameter if no valid data
        next
      }
      
      hist_plot <- ggplot(df_filtered, aes(x = .data[[param]])) +
        geom_histogram(aes(y = after_stat(count)), fill = "gray80", color = "gray30", bins = 30) +
        ggtitle(param) +
        xlab("Values") + ylab(NULL) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text = element_text(size = 9),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_blank(),
          panel.border = element_rect(color = "black", fill = NA),
          axis.line = element_line(color = "black")
        )

      # Get histogram data for scaling
      hist_data <- ggplot_build(hist_plot)$data[[1]]
      max_count <- max(hist_data$count)

      dens_data <- density(df_filtered[[param]], na.rm = TRUE)
      scale_factor <- max_count / max(dens_data$y)
      dens_df <- data.frame(x = dens_data$x, y = dens_data$y * scale_factor)

      p <- hist_plot +
        geom_area(
          data = dens_df,
          aes(x = x, y = y),
          fill = "#ADD8E6", alpha = 0.4
        ) +
        geom_line(
          data = dens_df,
          aes(x = x, y = y),
          color = "#0072B2", linewidth = 1
        ) +
        coord_cartesian(xlim = range(df_filtered[[param]], na.rm = TRUE)) +
        scale_y_continuous(
          breaks = function(x) {
            top <- max(x)
            c(0, round(top / 2), round(top))
          },
          expand = expansion(mult = c(0, 0.05))
        ) +
        # Add NA count annotation in top-right corner
        annotate("text", 
                 x = Inf, y = Inf, 
                 label = paste("NA =", na_count), 
                 hjust = 1.1, vjust = 1.5, 
                 size = 3, color = "darkred",
                 fontface = "bold")

    } else {
      # Count NA values for annotation
      na_count <- sum(is.na(df[[param]]))
      # Filter categorical data to remove any NA values for the histogram
      df_filtered <- df[!is.na(df[[param]]), ]
      
      if (nrow(df_filtered) == 0) {
        # Skip this parameter if no valid data
        next
      }
      
      p <- ggplot(df_filtered, aes(x = .data[[param]])) +
        geom_bar(fill = "gray80", color = "gray30", alpha = 0.9) +
        ggtitle(param) +
        xlab("Values") + ylab(NULL) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text = element_text(size = 9),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_blank(),
          panel.border = element_rect(color = "black", fill = NA),
          axis.line = element_line(color = "black")
        ) +
        scale_y_continuous(
          breaks = function(x) {
            top <- max(x)
            c(0, round(top / 2), round(top))
          },
          expand = expansion(mult = c(0, 0.05))
        ) +
        # Add NA count annotation in top-right corner
        annotate("text", 
                 x = Inf, y = Inf, 
                 label = paste("NA =", na_count), 
                 hjust = 1.1, vjust = 1.5, 
                 size = 3, color = "darkred",
                 fontface = "bold")
    }

    print(p)
  }

  dev.off()
}

# notlint end