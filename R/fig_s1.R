#' Generate Figure S1: Distribution of SIRI (Raw vs Log-transformed)
#'
#' Reproduces Supplementary Figure 1. Displays density plots for both the raw SIRI values
#' and the log-transformed values to demonstrate normalization.
#'
#' @param data A dataframe. Defaults to \code{pantheia::pantheia_data}.
#' @return A patchwork object.
#' @import ggplot2
#' @import patchwork
#' @export
fig_s1 <- function(data = pantheia::pantheia_data) {
  
  # 1. Verificación de paquetes
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' is required.")
  
  # 2. Verificación de variables
  if (!all(c("SIRI", "logsiri") %in% names(data))) {
    stop("❌ Error: Faltan las variables 'SIRI' o 'logsiri' en el dataset.")
  }
  
  # 3. Limpieza básica para evitar warnings de ggplot por NAs
  # Filtramos solo para el plot, sin afectar al objeto original
  df_plot <- data[!is.na(data$SIRI) & !is.na(data$logsiri), ]
  
  # --- Plot A: Raw Distribution ---
  p_raw <- ggplot2::ggplot(df_plot, ggplot2::aes(x = SIRI)) +
    ggplot2::geom_density(fill = "#2E86AB", alpha = 0.7, color = "#1A5276", linewidth = 1) +
    ggplot2::geom_rug(alpha = 0.3, color = "#1A5276") +
    ggplot2::labs(tag = "A",
                  title = "Raw Distribution",
                  x = "SIRI (Absolute Value)",
                  y = "Density") +
    ggplot2::xlim(0, 25) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.tag = ggplot2::element_text(face = "bold", size = 16),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # --- Plot B: Log Distribution ---
  p_log <- ggplot2::ggplot(df_plot, ggplot2::aes(x = logsiri)) +
    ggplot2::geom_density(fill = "#2E86AB", alpha = 0.7, color = "#1A5276", linewidth = 1) +
    ggplot2::geom_rug(alpha = 0.3, color = "#1A5276") +
    ggplot2::labs(tag = "B",
                  title = "Log-transformed Distribution",
                  x = "log(SIRI)",
                  y = "Density") +
    ggplot2::xlim(-2.5, 5) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.tag = ggplot2::element_text(face = "bold", size = 16),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(), # Estético: quitar eje Y del segundo plot
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  
  # --- Combinación con Patchwork ---
  p_combined <- (p_raw | p_log) +
    patchwork::plot_annotation(
      title = "Distribution of Systemic Inflammation Response Index (SIRI)",
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  return(p_combined)
}