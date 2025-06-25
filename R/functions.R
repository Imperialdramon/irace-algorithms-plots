# Leer parámetros sin usar locations
read_parameters_file <- function(parameters_file) {
  params <- read.csv2(parameters_file, stringsAsFactors = FALSE)
  required <- c("NAME", "TYPE", "VALUES_ARRAY")
  if (!all(required %in% colnames(params))) {
    stop("El archivo de parámetros debe contener columnas NAME, TYPE y VALUES_ARRAY.")
  }
  return(list(params = params))
}

# Verifica si un parámetro es numérico
is_numeric_param <- function(param_name, parameters_df) {
  type <- parameters_df$TYPE[parameters_df$NAME == param_name]
  return(length(type) == 1 && type %in% c("i", "r"))
}

# Función principal
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

  if (length(all_configs) == 0) stop("No se encontraron configuraciones.")

  df <- bind_rows(all_configs) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(Elite = as.logical(Elite))

  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  # NUEVA llamada al gráfico de coordenadas paralelas
  plot_parallel_coordinates(df, parameters, output_folder, escenario_name)

  # Gráficos de frecuencia
  plot_histograms(df, parameters, output_folder, escenario_name, "all")

  df_elite <- df %>% filter(Elite)
  if (nrow(df_elite) > 0) {
    plot_histograms(df_elite, parameters, output_folder, escenario_name, "elites")
  }
}

# Función para graficar coordenadas paralelas
plot_parallel_coordinates <- function(df, parameters_df, output_folder, escenario_name) {
  exclude_cols <- c(".ID.", "Elite", "Run", "ConfigName")

  defined_params <- parameters_df$NAME
  candidate_cols <- intersect(names(df), defined_params)

  parameter_cols <- candidate_cols[
    !(candidate_cols %in% exclude_cols) &
    sapply(df[candidate_cols], function(x) !all(is.na(x)))
  ]

  if (length(parameter_cols) < 2) {
    warning("No hay suficientes parámetros válidos para el gráfico de coordenadas paralelas.")
    return(NULL)
  }

  df_plot <- df[, c(parameter_cols, "Elite")]

  # Convertir todo a numérico (incluso factores/categóricos)
  df_plot[parameter_cols] <- lapply(parameter_cols, function(p) {
    tipo <- parameters_df$TYPE[parameters_df$NAME == p]
    val <- df_plot[[p]]

    if (tipo %in% c("c", "o")) {
      # Convertir factor con niveles ordenados
      factor(val, levels = unique(val), ordered = TRUE) |> as.numeric()
    } else if (tipo %in% c("i", "r")) {
      as.numeric(as.character(val))
    } else {
      val  # dejarlo como está si no se reconoce tipo
    }
  })

  # --- Gráfico todas las configuraciones ---
  p_all <- GGally::ggparcoord(df_plot,
                              columns = 1:length(parameter_cols),
                              groupColumn = "Elite",
                              scale = "globalminmax",
                              alphaLines = 0.4,
                              showPoints = FALSE) +
    ggtitle(paste("ParCoord -", escenario_name)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(file.path(output_folder, paste0("parallel_all_", escenario_name, ".pdf")),
         p_all, width = 12, height = 6)

  # --- Gráfico solo élites ---
  df_elite <- df_plot[df_plot$Elite, ]
  if (nrow(df_elite) > 0) {
    p_elite <- GGally::ggparcoord(df_elite,
                                  columns = 1:length(parameter_cols),
                                  groupColumn = "Elite",
                                  scale = "globalminmax",
                                  alphaLines = 0.4,
                                  showPoints = FALSE) +
      ggtitle(paste("ParCoord Elites -", escenario_name)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggsave(file.path(output_folder, paste0("parallel_elites_", escenario_name, ".pdf")),
           p_elite, width = 12, height = 6)
  }
}

# Función para plotear histogramas
plot_histograms <- function(df, parameters_df, output_folder, escenario_name, tag) {
  pdf(file.path(output_folder, paste0("freq_", tag, "_", escenario_name, ".pdf")), width = 7, height = 4)
  parameter_cols <- setdiff(names(df), c(".ID.", "Elite", "Run"))

  for (param in parameter_cols) {
    is_numeric <- is_numeric_param(param, parameters_df)

    if (is_numeric) {
      hist_plot <- ggplot(df, aes_string(x = param)) +
        geom_histogram(aes(y = ..count..), fill = "gray80", color = "gray30", bins = 30) +
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

      # Obtener datos de histograma para escalar
      hist_data <- ggplot_build(hist_plot)$data[[1]]
      max_count <- max(hist_data$count)

      dens_data <- density(df[[param]], na.rm = TRUE)
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
          color = "#0072B2", size = 1
        ) +
        coord_cartesian(xlim = range(df[[param]], na.rm = TRUE)) +
        scale_y_continuous(
          breaks = function(x) {
            top <- max(x)
            c(0, round(top / 2), round(top))
          },
          expand = expansion(mult = c(0, 0.05))
        )

    } else {
      p <- ggplot(df, aes_string(x = param)) +
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
        )
    }

    print(p)
  }

  dev.off()
}
