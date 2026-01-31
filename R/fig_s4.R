#' Generate Figure S4: Demographics and Lifestyle Factors
#'
#' Reproduces Supplementary Figure 4 (formerly S3 logic).
#' Includes boxplots for categorical variables and a scatter plot for age.
#' Allows manual p-value injection for trend tests (e.g., Smoking).
#'
#' @param data A dataframe. Defaults to \code{pantheia::pantheia_data}.
#' @return A patchwork object.
#' @import ggplot2
#' @import ggstatsplot
#' @import patchwork
#' @import ggsci
#' @import dplyr
#' @importFrom ggpubr ggscatter
#' @importFrom stats kruskal.test wilcox.test
#' @importFrom rlang as_label enquo
#' @export
fig_s4 <- function(data = pantheia::pantheia_data) {
  
  # Chequeo de paquetes
  required_pkgs <- c("ggstatsplot", "patchwork", "ggsci", "dplyr", "ggpubr")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop(paste("Package", pkg, "required."))
  }
  
  # --- FUNCIÓN ROBUSTA (Con soporte para P-Manual) ---
  plot_robust <- function(d, x_col, y_col, title_text, p_manual = NULL) {
    
    # 1. Limpieza
    df_clean <- dplyr::select(d, grp = {{x_col}}, val = {{y_col}})
    df_clean <- dplyr::filter(df_clean, !is.na(grp), !is.na(val))
    df_clean <- dplyr::filter(df_clean, is.finite(val))
    df_clean <- dplyr::mutate(df_clean, grp = as.factor(grp))
    
    if(nrow(df_clean) == 0) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = title_text))
    }
    
    # 2. Cálculo P-Valor (O uso del Manual)
    p_txt <- "p = NA"
    
    if (!is.null(p_manual)) {
      # Si el usuario da un P manual, lo usamos directo
      p_txt <- p_manual
    } else {
      # Cálculo automático
      unique_grps <- length(unique(df_clean$grp))
      if (unique_grps >= 2) {
        if (unique_grps == 2) {
          test <- try(stats::wilcox.test(val ~ grp, data = df_clean), silent = TRUE)
        } else {
          test <- try(stats::kruskal.test(val ~ grp, data = df_clean), silent = TRUE)
        }
        if (!inherits(test, "try-error")) {
          pval <- test$p.value
          p_txt <- ifelse(pval < 0.001, "p < 0.001", paste("p =", sprintf("%.3f", pval)))
        }
      }
    }
    
    # 3. Gráfico
    ggstatsplot::ggbetweenstats(
      data = df_clean,
      x = grp, 
      y = val,
      title = title_text,
      xlab = "",
      ylab = "log(SIRI)",
      type = "nonparametric",
      results.subtitle = FALSE, 
      package = "ggsci",
      palette = "nrc_npg",
      ggtheme = ggplot2::theme_classic(),
      point.args = list(alpha = 0.4, size = 1.5, position = ggplot2::position_jitterdodge(dodge.width = 0.6))
    ) +
      ggplot2::labs(subtitle = p_txt) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, color = "black", face = "italic", margin = ggplot2::margin(b = 5)),
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5) 
      )
  }
  
  # --- PANELES ---
  
  # 1. TABACO (Con P-valor manual de tendencia)

  p_smoke <- plot_robust(data, smoke_ordinal, logsiri, "Smoking Status",
                         p_manual = "Jonckheere-Terpstra: p = 0.025") +
    ggplot2::xlab("Smoking Habits")
  
  # 2. SEXO
 
  var_sex <- if("sex" %in% names(data)) data$sex else data$gender_patient__demography
  p_sex <- plot_robust(data, sex, logsiri, "Sex") # Asumiendo que 'sex' está limpio en tus datos
  
  # 3. ECOG
  p_ecog <- plot_robust(data, ecog_cat_3, logsiri, "ECOG Performance Status") + 
    ggplot2::xlab("ECOG Score")
  
  # 4. EJERCICIO
  p_exercise <- plot_robust(data, exercise, logsiri, "Physical Exercise")
  
  # 5. EDAD (SCATTER PLOT)
  # Usamos ggpubr::ggscatter como en tu script original
  # Filtramos NAs de edad/siri antes para evitar warnings
  df_age <- dplyr::filter(data, !is.na(edad), !is.na(logsiri), is.finite(logsiri))
  
  p_age <- ggpubr::ggscatter(df_age, x = "edad", y = "logsiri",
                             color = "#4DBBD5FF", alpha = 0.5, size = 1,
                             add = "reg.line", conf.int = TRUE,
                             cor.coef = TRUE, cor.method = "spearman",
                             xlab = "Age (years)", ylab = "log(SIRI)", title = "Age Correlation") +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5))
  
  # --- COMPOSICIÓN FINAL ---
  # Layout: (Sex | Age) / (ECOG | Smoke) / (Exercise | Spacer)
  
  fig_s4 <- (p_sex | p_age) / (p_ecog | p_smoke) / (p_exercise | patchwork::plot_spacer()) +
    patchwork::plot_annotation(
      tag_levels = 'A',
      title = "Figure S4. Demographics and Lifestyle Factors",
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold"))
    )
  
  return(fig_s4)
}