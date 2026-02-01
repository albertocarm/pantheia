#' Figure 2: Multivariable Analysis Forest Plot (Main Effects)
#'
#' Forest plot showing Hazard Ratios (PFS, OS) and Odds Ratios (Response)
#' derived from multivariable models without interaction terms.
#' The effect of logSIRI is calculated comparing the 75th vs 25th percentile
#' of the study population.
#'
#' @return A ggplot object.
#' @export
fig_2 <- function() {
  
  # =============================================================================
  # 1. SETUP & CONSTANTS
  # =============================================================================
  
  # Define exact quantiles from the study cohort (logsiri)
  val_q1 <- 0.1396771
  val_q3 <- 1.3868660
  
  # Median and approximate boundaries for spline knots
  median_val <- 0.7435348
  k_int <- median_val
  k_bnd <- c(-0.8, 2.5)
  
  # =============================================================================
  # 2. HELPER FUNCTIONS
  # =============================================================================
  
  # --- Calculate Main Effect (Q3 vs Q1) ---
  calc_main_effect <- function(mod, q1, q3, k_i, k_b, type="AFT") {
    idx <- grep("logsiri", names(mod$coef), ignore.case=TRUE)
    if(length(idx) == 0) return(list(est=NA, lo=NA, hi=NA))
    
    b <- mod$coef[idx]
    v <- mod$vcov[idx, idx, drop=FALSE]
    
    if(length(idx) == 1) { 
      # Linear model calculation
      diff_val <- as.numeric(q3 - q1)
      delta <- diff_val * b
      var   <- (diff_val^2) * v
    } else { 
      # Restricted cubic spline calculation
      tryCatch({
        b1 <- splines::ns(q1, knots=k_i, Boundary.knots=k_b)
        b3 <- splines::ns(q3, knots=k_i, Boundary.knots=k_b)
        diff_b <- as.numeric(b3 - b1)
        if(length(diff_b) != length(b)) diff_b <- rep(0, length(b))
        delta <- sum(diff_b * b)
        var   <- as.numeric(t(diff_b) %*% v %*% diff_b)
      }, error = function(e) {
        # Fallback to linear calculation if spline basis fails
        delta <<- as.numeric(q3 - q1) * b[1]
        var   <<- (as.numeric(q3 - q1)^2) * v[1,1]
      })
    }
    
    se <- sqrt(as.numeric(var))
    
    if(type=="AFT") {
      # Accelerated Failure Time models (survreg)
      sc <- mod$scale; if(is.null(sc)) sc <- 1
      log_est <- -delta / sc
      se_est  <- se / sc
    } else {
      # Logistic regression or Cox
      log_est <- delta
      se_est  <- se
    }
    
    list(est=exp(log_est), lo=exp(log_est-1.96*se_est), hi=exp(log_est+1.96*se_est))
  }
  
  # --- Build Data Frame Row from Model Object ---
  build_row <- function(model, label, outcome, type="AFT") {
    if(is.null(model$coef) || is.null(model$vcov)) stop("Model object missing coefficients or vcov")
    
    betas <- model$coef
    se    <- sqrt(diag(model$vcov))
    
    df <- data.frame(term = names(betas), beta = betas, se = se, stringsAsFactors = FALSE)
    
    # Filter out Intercept, raw logsiri terms, and stratification variables (e.g., Age)
    df <- df[!grepl("Intercept", df$term, ignore.case = TRUE), ]
    df <- df[!grepl("logsiri", df$term, ignore.case = TRUE), ]
    df <- df[!grepl("edad|age", df$term, ignore.case = TRUE), ]
    
    # Standardize Variable Labels for Publication
    df$var_label <- df$term
    df$var_label[grepl("diam", df$term)] <- "Tumor Diameter (>5 vs <=5cm)"
    df$var_label[grepl("ecog.*1", df$term)] <- "ECOG PS (1 vs 0)"
    df$var_label[grepl("ecog.*2", df$term)] <- "ECOG PS (>=2 vs 0)"
    df$var_label[grepl("CACS", df$term)] <- "Cachexia (Yes vs No)"
    df$var_label[grepl("Gem-Abraxane", df$term)] <- "Gem/Nab-P vs FOLFIRINOX"
    df$var_label[grepl("Gemcitabine", df$term)] <- "Gemcitabine vs FOLFIRINOX"
    df$var_label[grepl("Other", df$term)] <- "Other vs FOLFIRINOX"
    
    # Convert coefficients to HR/OR
    if(type=="AFT") {
      sc <- model$scale; if(is.null(sc)) sc <- 1
      df$log_est <- -df$beta / sc
      df$se_est  <- df$se / sc
    } else {
      df$log_est <- df$beta
      df$se_est  <- df$se
    }
    
    df$estimate <- exp(df$log_est)
    df$lower    <- exp(df$log_est - 1.96 * df$se_est)
    df$upper    <- exp(df$log_est + 1.96 * df$se_est)
    
    # Calculate specific logSIRI effect
    siri_res <- calc_main_effect(model, val_q1, val_q3, k_int, k_bnd, type)
    
    row_siri <- data.frame(
      term = "logsiri_main",
      var_label = "logSIRI (Q3 vs Q1)",
      estimate = siri_res$est,
      lower = siri_res$lo,
      upper = siri_res$hi,
      stringsAsFactors = FALSE
    )
    
    # Ensure column matching for binding
    df_subset <- df[, c("term", "var_label", "estimate", "lower", "upper")]
    
    final <- rbind(row_siri, df_subset)
    final$Outcome <- outcome
    
    return(final)
  }
  
  # =============================================================================
  # 3. DATA GENERATION
  # =============================================================================
  
  # Access internal package models directly
  df_pfs  <- build_row(FINAL_PFS_RUBIN_sin_interaccion, "PFS", "PFS (HR)", "AFT")
  df_os   <- build_row(FINAL_OS_RUBIN_sin_interaccion,  "OS",  "OS (HR)",  "AFT")
  df_resp <- build_row(FINAL_RESP_RUBIN_sin_interaccion,"Resp","Response (OR)", "LOGIT")
  
  # Combine all outcomes
  df_plot <- rbind(df_pfs, df_os, df_resp)
  
  # Format estimate labels
  df_plot$label_text <- sprintf("%.2f (%.2f-%.2f)", df_plot$estimate, df_plot$lower, df_plot$upper)
  
  # Set Factor Levels for Ordering
  orden_vars <- unique(df_plot$var_label)
  orden_vars <- c(setdiff(orden_vars, "logSIRI (Q3 vs Q1)"), "logSIRI (Q3 vs Q1)")
  df_plot$var_label <- factor(df_plot$var_label, levels = orden_vars)
  
  df_plot$Outcome <- factor(df_plot$Outcome, levels = c("OS (HR)", "PFS (HR)", "Response (OR)"))
  
  # =============================================================================
  # 4. PLOTTING
  # =============================================================================
  
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = estimate, y = var_label, color = Outcome)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "solid", color = "grey30", linewidth = 0.4) +
    
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0.2, linewidth = 0.7) +
    ggplot2::geom_point(size = 3, shape = 15) +
    
    # Add text labels aligned to the right
    ggplot2::geom_text(ggplot2::aes(label = label_text, x = upper),
                       hjust = -0.2, size = 3, show.legend = FALSE, family = "sans") +
    
    ggplot2::facet_wrap(~ Outcome, scales = "free_x") +
    
    # Scales and Colors
    ggplot2::scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10)) +
    ggplot2::scale_color_manual(values = c("PFS (HR)"="#377eb8", "OS (HR)"="#e41a1c", "Response (OR)"="#4daf4a")) +
    
    # Labels and Titles
    ggplot2::labs(title = "Multivariable Analysis of Outcomes",
                  subtitle = "Forest plot showing Hazard Ratios (HR) and Odds Ratios (OR)",
                  x = "Estimate (95% CI)",
                  y = NULL) +
    
    # Theme Settings
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey95", color = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major.y = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, color = "black"),
      axis.text.x = ggplot2::element_text(size = 9, color = "grey30"),
      legend.position = "none",
      plot.margin = ggplot2::margin(10, 20, 10, 10)
    ) +
    
    # Expansion to fit text labels
    ggplot2::scale_x_continuous(trans = "log10",
                                breaks = c(0.2, 0.5, 1, 2, 5, 10),
                                expand = ggplot2::expansion(mult = c(0.05, 0.6)))
  
  return(p)
}