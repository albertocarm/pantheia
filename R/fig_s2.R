#' Generate Figure S2: Association between SIRI and Cancer-Related Symptoms
#'
#' Reproduces Supplementary Figure 2 using cleaned clinical variables.
#' This figure analyzes the association between log(SIRI) levels and the presence
#' of various cancer-related symptoms (Asthenia, Anorexia, Weight Loss, Cachexia,
#' Tumor Pain, and CACS Syndrome).
#'
#' @param data A dataframe. Defaults to \code{pantheia::pantheia_data}.
#' @return A patchwork object.
#' @import ggplot2
#' @import ggstatsplot
#' @import patchwork
#' @import ggsci
#' @import dplyr
#' @importFrom stats kruskal.test wilcox.test
#' @importFrom rlang as_label enquo
#' @export
fig_s2 <- function(data = pantheia::pantheia_data) {
  
  # 1. Variable Verification
  required_vars <- c("logsiri", "asthenia", "anorexia", "weight_loss_bin", 
                     "cachexia", "symp_tumorpain", "CACS_syndrome")
  
  missing_vars <- setdiff(required_vars, names(data))
  
  if (length(missing_vars) > 0) {
    stop(paste("Error: The following variables are missing from the dataset:", paste(missing_vars, collapse = ", ")))
  }
  
  # Suppress warnings for cleaner output (e.g., ties in Wilcoxon test)
  suppressWarnings({
    
    # --- ROBUST PLOTTING FUNCTION ---
    plot_robust <- function(d, x_col, y_col, title_text) {
      
      # Standard Data Cleaning
      df_clean <- dplyr::select(d, grp = {{x_col}}, val = {{y_col}})
      df_clean <- dplyr::filter(df_clean, !is.na(grp), !is.na(val))
      df_clean <- dplyr::filter(df_clean, is.finite(val))
      df_clean <- dplyr::mutate(df_clean, grp = as.factor(grp))
      
      if(nrow(df_clean) == 0) {
        return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = paste(title_text, "(No Data)")))
      }
      
      # P-value Calculation
      p_txt <- "p = NA"
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
      
      # Plot Generation
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
    
    # --- PANEL GENERATION ---
    p1 <- plot_robust(data, asthenia,        logsiri, "Asthenia")
    p2 <- plot_robust(data, anorexia,        logsiri, "Anorexia")
    p3 <- plot_robust(data, weight_loss_bin, logsiri, "Weight Loss")
    p4 <- plot_robust(data, cachexia,        logsiri, "Cachexia")
    p5 <- plot_robust(data, symp_tumorpain,  logsiri, "Tumor Pain")
    p6 <- plot_robust(data, CACS_syndrome,   logsiri, "Anorexia-Cachexia Syndrome")
    
    # --- FINAL COMPOSITION ---
    combined_plot <- (p1 | p2 | p3) / (p4 | p6 | p5) +
      patchwork::plot_annotation(
        tag_levels = 'A',
        title = "Figure S2. Association between SIRI and Cancer-Related Symptoms",
        theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold"))
      )
    
  }) # End suppressWarnings
  
  return(combined_plot)
}