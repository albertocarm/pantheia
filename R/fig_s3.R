#' Generate Figure S3: Association between SIRI and Tumor Burden
#'
#' Reproduces Supplementary Figure 3. This figure analyzes the association between 
#' log(SIRI) levels and various markers of tumor burden (Liver Metastases, Lung Metastases, 
#' Peritoneal Metastases, Liver Tumor Burden, VTE, and Obstructive Jaundice).
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
fig_s3 <- function(data = pantheia::pantheia_data) {
  
  # 1. Variable Verification
  required_vars <- c("logsiri", "liver_met", "lung_met", "peritoneum_met", 
                     "liver_burden", "vte_basal", "obstructive_jaundice")
  
  missing_vars <- setdiff(required_vars, names(data))
  
  if (length(missing_vars) > 0) {
    stop(paste("Error: The following variables are missing from the dataset:", paste(missing_vars, collapse = ", ")))
  }
  
  # Suppress warnings for cleaner output (e.g., ties in Wilcoxon test)
  suppressWarnings({
    
    # --- ROBUST PLOTTING FUNCTION ---
    plot_robust <- function(d, x_col, y_col, title_text) {
      
      # Standard Data Cleaning (Avoiding pipes for maximum stability)
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
    p7  <- plot_robust(data, liver_met,            logsiri, "Liver Metastases")
    p8  <- plot_robust(data, lung_met,             logsiri, "Lung Metastases")
    p9  <- plot_robust(data, peritoneum_met,       logsiri, "Peritoneal Metastases")
    p10 <- plot_robust(data, liver_burden,         logsiri, "Liver Tumor Burden")
    p11 <- plot_robust(data, vte_basal,            logsiri, "Venous Thromboembolism")
    p12 <- plot_robust(data, obstructive_jaundice, logsiri, "Obstructive Jaundice")
    
    # --- FINAL COMPOSITION ---
    combined_plot <- (p7 | p8 | p9) / (p10 | p11 | p12) +
      patchwork::plot_annotation(
        tag_levels = 'A',
        title = "Figure S3. Association between SIRI and Tumor Burden",
        theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold"))
      )
    
  }) # End suppressWarnings
  
  return(combined_plot)
}