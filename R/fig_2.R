#' Generate Figure 2: Interaction Curves (Regimen vs logSIRI)
#'
#' Reproduces Figure 2. Plots the estimated HR (PFS, OS) and OR (Response) against logSIRI,
#' stratified by chemotherapy regimen. Includes panel-specific p-values for the main effect
#' and nonlinearity, derived directly from the internal Rubin models.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom stats pchisq
#' @export
fig_2 <- function() {
  
  # 1. Check Internal Models
  if (!exists("mod_pfs") || !exists("mod_os") || !exists("mod_resp")) {
    stop("Internal models (mod_pfs, mod_os, mod_resp) not found. Ensure sysdata.rda is loaded.")
  }
  
  # ===========================================================================
  # A. DATA GENERATION
  # ===========================================================================
  
  # Grid Setup
  x_grid <- seq(-2, 2, length.out = 100)
  q2 <- 0.04 # Default median
  if (!is.null(mod_pfs$knots)) q2 <- unname(mod_pfs$knots["50%"])
  
  # Internal helper to generate curve data using package utilities
  calc_curve_internal <- function(mod, kind, endpoint) {
    regimens <- c("Total", mod$xlevels$regimen_cat)
    out_list <- list()
    diam0 <- "Low"; ecog0 <- "0"; cacs0 <- "No"
    
    # Pre-calculate reference rows
    nd_ref_list_total <- lapply(mod$xlevels$regimen_cat, function(r) ref_row(mod, q2, r, diam0, ecog0, cacs0))
    
    for (rp in regimens) {
      if (rp == "Total") {
        # Total = Marginal average across regimens
        nd_ref_list <- nd_ref_list_total
      } else {
        nd_ref <- ref_row(mod, q2, rp, diam0, ecog0, cacs0)
      }
      
      ests <- numeric(length(x_grid)); los <- numeric(length(x_grid)); his <- numeric(length(x_grid))
      
      for (i in seq_along(x_grid)) {
        x <- x_grid[i]
        if (rp == "Total") {
          effs <- sapply(mod$xlevels$regimen_cat, function(r) {
            nd_x <- ref_row(mod, x, r, diam0, ecog0, cacs0)
            cc <- lp_contrast_mod_w(mod, nd_ref_list[[which(mod$xlevels$regimen_cat == r)]], nd_x)
            ratio_from_delta(mod, cc$delta, cc$var_delta, kind)[1]
          })
          ests[i] <- mean(effs)
          los[i] <- NA; his[i] <- NA
        } else {
          nd_x <- ref_row(mod, x, rp, diam0, ecog0, cacs0)
          cc <- lp_contrast_mod_w(mod, nd_ref, nd_x)
          rr <- ratio_from_delta(mod, cc$delta, cc$var_delta, kind)
          ests[i] <- rr[1]; los[i] <- rr[2]; his[i] <- rr[3]
        }
      }
      out_list[[rp]] <- data.frame(regimen = rp, endpoint = endpoint, logsiri = x_grid, est = ests, lo = los, hi = his)
    }
    do.call(rbind, out_list)
  }
  
  # Generate Data
  df1 <- calc_curve_internal(mod_pfs, "AFT_HR", "PFS")
  df2 <- calc_curve_internal(mod_os, "AFT_HR", "OS")
  df3 <- calc_curve_internal(mod_resp, "LOGIT_OR", "Response")
  df_curves <- rbind(df1, df2, df3)
  
  # ===========================================================================
  # B. DATA PROCESSING
  # ===========================================================================
  
  # Drop "Other" regimen
  .drop_other <- trimws(as.character(df_curves$regimen)) == "Other"
  df_curves <- df_curves[!.drop_other, , drop = FALSE]
  if (is.factor(df_curves$regimen)) df_curves$regimen <- droplevels(df_curves$regimen)
  
  # Factor Ordering
  df_curves$endpoint <- factor(df_curves$endpoint, levels = c("PFS", "OS", "Response"))
  
  if (!("Total" %in% as.character(df_curves$regimen))) stop("Row 'Total' not found in data.")
  
  reg_levels <- levels(df_curves$regimen)
  if (is.null(reg_levels)) reg_levels <- unique(as.character(df_curves$regimen))
  
  # Ensure "Total" is the first level
  reg_levels <- c("Total", setdiff(reg_levels, "Total"))
  df_curves$regimen <- factor(df_curves$regimen, levels = reg_levels)
  
  # ===========================================================================
  # C. STATISTICAL HELPERS (Local)
  # ===========================================================================
  
  fmt_p <- function(p) {
    p <- as.numeric(p)
    out <- character(length(p))
    out[is.na(p)] <- "NA"
    out[!is.na(p) & p < 0.001] <- "<0.001"
    idx <- !is.na(p) & p >= 0.001
    out[idx] <- sprintf("%.3f", p[idx])
    out
  }
  
  wald_p <- function(mod, coef_names) {
    b0 <- mod$coef
    V0 <- as.matrix(mod$vcov)
    
    keep <- coef_names[coef_names %in% names(b0)]
    if (length(keep) == 0) return(NA_real_)
    keep <- keep[!is.na(b0[keep])]
    if (length(keep) == 0) return(NA_real_)
    
    b <- b0[keep]
    V <- V0[keep, keep, drop = FALSE]
    
    stat <- tryCatch({
      as.numeric(t(b) %*% qr.solve(V, b, tol = 1e-12))
    }, error = function(e) {
      ev <- eigen(V, symmetric = TRUE)
      ev$values[ev$values < 1e-12] <- 1e-12
      Vinv <- ev$vectors %*% diag(1 / ev$values) %*% t(ev$vectors)
      as.numeric(t(b) %*% Vinv %*% b)
    })
    
    df <- length(b)
    stats::pchisq(stat, df = df, lower.tail = FALSE)
  }
  
  term_int_for_reg <- function(mod, reg, endpoint) {
    nm <- names(mod$coef)
    reg_esc <- gsub("([\\^\\$\\.|\\(\\)\\[\\]\\*\\+\\?\\\\])", "\\\\\\1", reg)
    # Search for interaction patterns (A:B or B:A)
    pat1 <- paste0("^regimen_cat", reg_esc, ":logsiri$")
    pat2 <- paste0("^logsiri:regimen_cat", reg_esc, "$")
    found <- grep(pat1, nm, value = TRUE)
    if (length(found) == 0) found <- grep(pat2, nm, value = TRUE)
    return(found)
  }
  
  panel_pvals <- function(mod, endpoint, reg_levels) {
    nm <- names(mod$coef)
    
    # 1. Detect Main logSIRI Terms (Spline or Linear)
    spline_terms <- grep("^ns\\(logsiri", nm, value = TRUE)
    if (length(spline_terms) > 0) {
      main_logsiri <- spline_terms
      is_nonlinear <- TRUE
    } else {
      main_logsiri <- grep("^logsiri$", nm, value = TRUE)
      is_nonlinear <- FALSE
    }
    
    # 2. Calculate Nonlinearity P-value (Global)
    if (is_nonlinear && length(main_logsiri) >= 2) {
      # Test higher order terms
      p_nl_total <- wald_p(mod, main_logsiri[-1])
    } else {
      p_nl_total <- NA_real_
    }
    
    # 3. Calculate Overall P-value (Total)
    all_logsiri_terms <- grep("logsiri", nm, value = TRUE)
    p_tot <- wald_p(mod, all_logsiri_terms)
    
    out <- data.frame(
      regimen = c("Total", reg_levels[reg_levels != "Total"]),
      endpoint = endpoint,
      p_overall = NA_real_,
      p_nl = NA_real_,
      stringsAsFactors = FALSE
    )
    
    # Fill Total
    out$p_overall[out$regimen == "Total"] <- p_tot
    out$p_nl[out$regimen == "Total"] <- p_nl_total
    
    # Fill Regimens
    regs <- reg_levels[reg_levels != "Total"]
    for (r in regs) {
      int_terms <- term_int_for_reg(mod, r, endpoint)
      test_terms <- unique(c(main_logsiri, int_terms))
      out$p_overall[out$regimen == r] <- wald_p(mod, test_terms)
      
      if (is_nonlinear && length(main_logsiri) >= 2) {
        out$p_nl[out$regimen == r] <- wald_p(mod, main_logsiri[-1])
      } else {
        out$p_nl[out$regimen == r] <- NA_real_
      }
    }
    
    out$regimen <- factor(out$regimen, levels = reg_levels)
    out$endpoint <- factor(out$endpoint, levels = levels(df_curves$endpoint))
    out
  }
  
  # ===========================================================================
  # D. ANNOTATION GENERATION
  # ===========================================================================
  
  df_p_pan <- rbind(
    panel_pvals(mod_pfs,  "PFS",      reg_levels),
    panel_pvals(mod_os,   "OS",       reg_levels),
    panel_pvals(mod_resp, "Response", reg_levels)
  )
  
  # Label Logic
  df_p_pan$lab <- ifelse(
    !is.na(df_p_pan$p_nl),
    paste0("logSIRI p = ", fmt_p(df_p_pan$p_overall), "\nNonlinearity p = ", fmt_p(df_p_pan$p_nl)),
    paste0("logSIRI p = ", fmt_p(df_p_pan$p_overall), "\nLinear model")
  )
  
  df_p_pan$x <- -Inf
  df_p_pan$y <- Inf
  
  # ===========================================================================
  # E. PLOTTING
  # ===========================================================================
  
  # Labeller for Gemcitabine
  lab_regimen <- function(x) {
    x0 <- as.character(x)
    x0[grepl("^gemcitabine\\s+mono$", trimws(x0), ignore.case = TRUE)] <- "Gemcitabine"
    x0
  }
  
  # Aesthetics
  pal_line <- c("PFS" = "#1E5AA8", "OS" = "#35686C", "Response" = "#5A6C80")
  pal_fill <- pal_line
  ymin_plot <- 0.1
  ymax_plot <- 7
  
  p <- ggplot2::ggplot(df_curves, ggplot2::aes(x = logsiri, y = est)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi, fill = endpoint), alpha = 0.18, color = NA) +
    ggplot2::geom_line(ggplot2::aes(color = endpoint), linewidth = 0.95) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey45", linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = q2, linetype = "dotted", color = "grey55", linewidth = 0.55) +
    ggplot2::geom_text(
      data = df_p_pan,
      ggplot2::aes(x = x, y = y, label = lab),
      inherit.aes = FALSE,
      hjust = -0.02, vjust = 1.08,
      size = 3.2,
      color = "grey15",
      lineheight = 0.95
    ) +
    ggplot2::facet_grid(regimen ~ endpoint, labeller = ggplot2::labeller(regimen = lab_regimen)) +
    ggplot2::scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3)) +
    ggplot2::scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 4, 7)) +
    ggplot2::coord_cartesian(ylim = c(ymin_plot, ymax_plot), clip = "off") +
    ggplot2::scale_color_manual(values = pal_line, guide = "none") +
    ggplot2::scale_fill_manual(values = pal_fill, guide = "none") +
    ggplot2::labs(
      x = "logSIRI",
      y = "HR / OR relative to logSIRI median (within panel)",
      title = "Effect of logSIRI by regimen (interaction) across endpoints",
      subtitle = "Rows: Total (equal-weight marginal across regimens) + each regimen. Columns: PFS HR, OS HR, Response OR."
    ) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey95", color = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 12.5),
      axis.title = ggplot2::element_text(size = 13),
      axis.text  = ggplot2::element_text(size = 11.5),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
  
  return(p)
}