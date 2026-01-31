#' Generate Figure 1: Survival Analysis and Response Rates
#'
#' Reproduces Figure 1 (A: Overall Survival, B: Progression-Free Survival, C: Response Rates by SIRI).
#' Uses Kaplan-Meier curves and a faceted bar plot for response rates.
#' Warnings are suppressed during plot generation.
#'
#' @param data A dataframe. Defaults to \code{pantheia::pantheia_data}.
#' @return A patchwork object (combined plot).
#' @import ggplot2
#' @import survminer
#' @import survival
#' @import ggsci
#' @import patchwork
#' @import dplyr
#' @importFrom scales percent
#' @export
fig_1 <- function(data = pantheia::pantheia_data) {
  
  # 1. Package and Variable Verification
  required_pkgs <- c("survminer", "survival", "ggsci", "patchwork", "dplyr", "scales")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop(paste("Package", pkg, "is required."))
  }
  
  req_vars <- c("os_time", "os_event", "pfs_time1l", "pfs_event", "SIRI_cat", "recist_plot", "regimen_cat_clean")
  if (!all(req_vars %in% names(data))) {
    stop("Error: Missing necessary survival or response variables in the dataset.")
  }
  
  # Suppress warnings for cleaner output
  suppressWarnings({
    
    # --- PART A: OVERALL SURVIVAL (OS) ---
    fit_os <- survival::survfit(survival::Surv(os_time, os_event) ~ SIRI_cat, data = data)
    
    p_os <- survminer::ggsurvplot(
      fit_os, data = data,
      risk.table = TRUE,
      xlim = c(0, 66), break.time.by = 6,
      palette = "jco",
      legend = c(0.8, 0.85), legend.title = "SIRI Status",
      censor.shape = 124, censor.size = 2,
      conf.int = FALSE,
      pval = TRUE, pval.coord = c(0, 0.1),
      xlab = "Time (months)", ylab = "Overall Survival",
      tables.y.text = FALSE, risk.table.y.text = FALSE, risk.table.y.text.col = TRUE,
      legend.labs = c("<= 2.3", "> 2.3"), # Updated labels
      risk.table.title = "At risk",
      surv.median.line = "hv",
      tables.theme = survminer::theme_cleantable(),
      ggtheme = ggplot2::theme_classic()
    )
    
    # Fine-tune axes
    p_os$plot <- p_os$plot + ggplot2::scale_x_continuous(breaks = seq(0, 66, 6)) + ggplot2::labs(tag = "A")
    p_os$table <- p_os$table + ggplot2::scale_x_continuous(breaks = seq(0, 66, 6), limits = c(0, 66))
    
    # --- PART B: PROGRESSION-FREE SURVIVAL (PFS) ---
    # Filter for PFS > 0
    data_pfs <- dplyr::filter(data, pfs_time1l > 0)
    fit_pfs <- survival::survfit(survival::Surv(pfs_time1l, pfs_event) ~ SIRI_cat, data = data_pfs)
    
    p_pfs <- survminer::ggsurvplot(
      fit_pfs, data = data_pfs,
      risk.table = TRUE,
      xlim = c(0, 24), break.time.by = 3,
      palette = "jco",
      legend = c(0.8, 0.85), legend.title = "SIRI Status",
      censor.shape = 124, censor.size = 2,
      conf.int = FALSE,
      pval = TRUE, pval.coord = c(0, 0.05),
      xlab = "Time (months)", ylab = "Progression-Free Survival",
      tables.y.text = FALSE, risk.table.y.text = FALSE, risk.table.y.text.col = TRUE,
      legend.labs = c("<= 2.3", "> 2.3"), # Updated labels
      risk.table.title = "At risk",
      surv.median.line = "hv",
      tables.theme = survminer::theme_cleantable(),
      ggtheme = ggplot2::theme_classic()
    )
    
    p_pfs$plot <- p_pfs$plot + ggplot2::scale_x_continuous(breaks = seq(0, 24, 3)) + ggplot2::labs(tag = "B")
    p_pfs$table <- p_pfs$table + ggplot2::scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24))
    
    # --- PART C: RESPONSE RATE BAR PLOT ---
    
    # Dynamic Data Preparation (All Patients + Subgroups)
    df_overall <- data %>% dplyr::mutate(panel_group = "All Patients")
    df_subgroups <- data %>% dplyr::mutate(panel_group = as.character(regimen_cat_clean))
    df_combined <- dplyr::bind_rows(df_overall, df_subgroups)
    
    # Factor ordering
    df_combined$panel_group <- factor(df_combined$panel_group,
                                      levels = c("All Patients", "FOLFIRINOX", "Gem-Abraxane", "Gemcitabine", "Other"))
    
    # Filter NAs
    df_combined <- dplyr::filter(df_combined, !is.na(recist_plot), !is.na(SIRI_cat), !is.na(panel_group))
    
    # Calculate P-values (Chi-squared)
    p_values <- df_combined %>%
      dplyr::group_by(panel_group) %>%
      dplyr::summarise(
        p_val = tryCatch(stats::chisq.test(table(SIRI_cat, recist_plot))$p.value, error = function(e) NA),
        .groups = 'drop'
      ) %>%
      dplyr::mutate(
        label = ifelse(!is.na(p_val) & p_val < 0.001, "p < 0.001", 
                       ifelse(!is.na(p_val), paste("p =", round(p_val, 3)), ""))
      )
    
    # Bar Plot
    p_bars <- ggplot2::ggplot(df_combined, ggplot2::aes(x = SIRI_cat, fill = recist_plot)) +
      ggplot2::geom_bar(position = "fill", width = 0.8, color = "black") +
      ggplot2::facet_grid(~ panel_group, scales = "free_x", space = "free_x") +
      
      # P-value Labels
      ggplot2::geom_text(data = p_values, ggplot2::aes(x = 1.5, y = 1.05, label = label),
                         inherit.aes = FALSE, size = 3, fontface = "bold") +
      
      ggplot2::scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0, 1.1)) +
      ggsci::scale_fill_jco() +
      ggplot2::theme_classic() +
      ggplot2::labs(tag = "C", x = "SIRI Status", y = "% Response", fill = "Response") +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "#EFEFEF", color = NA),
        strip.text = ggplot2::element_text(face = "bold", size = 9),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.tag = ggplot2::element_text(face = "bold", size = 16)
      )
    
    # --- FINAL COMPOSITION ---
    
    # Extract plots from survminer objects
    g_os <- p_os$plot
    g_os_t <- p_os$table
    g_pfs <- p_pfs$plot
    g_pfs_t <- p_pfs$table
    
    layout_design <- "
    AABB
    CCDD
    EEEE
    EEEE
    "
    
    final_plot <- (g_os + g_pfs + g_os_t + g_pfs_t + p_bars) +
      patchwork::plot_layout(design = layout_design, heights = c(1, 0.3, 1, 0.2))
    
  }) # End suppressWarnings
  
  return(final_plot)
}