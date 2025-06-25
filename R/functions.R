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
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Instala el paquete MASS con install.packages('MASS')")
  }

  exclude_cols <- c(".ID.", "Elite", "Run", "ConfigName")
  defined <- intersect(names(df), parameters_df$NAME)
  parameter_cols <- defined[
    !(defined %in% exclude_cols) &
    sapply(df[defined], function(x) !all(is.na(x)))
  ]

  if (length(parameter_cols) < 2) {
    warning("No hay suficientes parámetros válidos para parcoord.")
    return(NULL)
  }

  # Crear matrix numérico para parcoord
  df_num <- df[parameter_cols]
  for (p in parameter_cols) {
    tipo <- parameters_df$TYPE[parameters_df$NAME == p]
    val <- df_num[[p]]
    if (tipo %in% c("c", "o")) {
      df_num[[p]] <- as.numeric(factor(val, levels = unique(na.omit(val)), ordered = TRUE))
    } else if (tipo %in% c("i", "r")) {
      df_num[[p]] <- as.numeric(as.character(val))
    }
    # Los NA se mantienen, produciendo huecos en las líneas
  }

  # Colores suaves para élites / no élites
  cols <- ifelse(df$Elite,
                 grDevices::adjustcolor("#0072B2", alpha.f = 0.6),
                 grDevices::adjustcolor("gray80", alpha.f = 0.3))

  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

  # Preparar rangos para asegurar visibilidad completa de los ejes
  rngs <- apply(df_num, 2, function(v) range(v, na.rm = TRUE))
  # parcoord rescalea internamente, pero dejamos rx por claridad
  rx <- rngs

  # — Gráfico completo —
  grDevices::pdf(file.path(output_folder, paste0("parallel_all_", escenario_name, ".pdf")),
                 width = 12, height = 6)
  MASS::parcoord(df_num, col = cols, var.label = TRUE, lty = 1, rx = rx)
  title(main = paste("ParCoord -", escenario_name))
  grDevices::dev.off()

  # — Gráfico solo élites —
  elite_idx <- which(df$Elite)
  if (length(elite_idx) > 0) {
    grDevices::pdf(file.path(output_folder, paste0("parallel_elites_", escenario_name, ".pdf")),
                   width = 12, height = 6)
    MASS::parcoord(df_num[elite_idx, , drop = FALSE],
                   col = cols[elite_idx],
                   var.label = TRUE, lty = 1, rx = rx)
    title(main = paste("ParCoord Elites -", escenario_name))
    grDevices::dev.off()
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
          color = "#0072B2", linewidth = 1
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
