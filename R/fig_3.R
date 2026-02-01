#' Generate Figure 2: Interaction Curves (Regimen vs logSIRI)
#'
#' Reproduces Figure 2. Plots the estimated HR (PFS, OS) and OR (Response) against logSIRI,
#' stratified by chemotherapy regimen. Includes panel-specific p-values for the main effect
#' and nonlinearity, derived directly from the internal Rubin models.
#' @export

fig_3 <- function() {
  
  # --- 1. Asignación directa ---
  # Al ser un paquete, R busca estas variables automáticamente en sysdata.rda
  # Eliminamos la comprobación 'exists()' que causaba el error falso.
  mod_pfs_int  <- mod_pfs
  mod_os_int   <- mod_os
  mod_resp_int <- mod_resp
  mod_pfs_main <- FINAL_PFS_RUBIN_sin_interaccion
  mod_os_main  <- FINAL_OS_RUBIN_sin_interaccion
  mod_resp_main <- FINAL_RESP_RUBIN_sin_interaccion
  
  # --- 2. Setup Grid and Reference ---
  x_grid <- seq(-2, 2, length.out = 100)
  
  # Reference value (median) for HR=1 or OR=1
  q2 <- 0.04
  if (!is.null(mod_pfs_main$knots)) {
    q2 <- unname(mod_pfs_main$knots["50%"])
  }
  
  # --- 3. Helper Functions ---
  
  # Helper: Main Effects HR (Splines/AFT)
  calc_main_hr <- function(mod, x_vals, ref_val) {
    betas <- mod$coef
    V <- mod$vcov
    scale <- mod$scale 
    
    idx <- grep("logsiri", names(betas), ignore.case = TRUE)
    b_siri <- betas[idx]
    v_siri <- V[idx, idx]
    
    if(!is.null(mod$knots)) {
      k_vec <- mod$knots
      X_seq <- splines::ns(x_vals, knots=k_vec["50%"], Boundary.knots=k_vec[c(1,3)])
      X_ref <- splines::ns(ref_val, knots=k_vec["50%"], Boundary.knots=k_vec[c(1,3)])
    } else {
      X_seq <- matrix(x_vals, ncol=1)
      X_ref <- matrix(ref_val, ncol=1)
    }
    
    X_diff <- sweep(X_seq, 2, as.vector(X_ref), "-")
    lp <- X_diff %*% b_siri
    se <- sqrt(rowSums((X_diff %*% v_siri) * X_diff))
    
    log_hr <- -lp / scale
    se_hr <- se / scale
    
    list(est = exp(log_hr), lo = exp(log_hr - 1.96 * se_hr), hi = exp(log_hr + 1.96 * se_hr))
  }
  
  # Helper: Main Effects OR (Logistic)
  calc_main_or <- function(mod, x_vals, ref_val) {
    betas <- mod$coef
    V <- mod$vcov
    
    idx <- grep("logsiri", names(betas), ignore.case = TRUE)
    b_siri <- betas[idx]
    v_siri <- V[idx, idx]
    
    diffs <- x_vals - ref_val
    lp_diff <- diffs * b_siri
    se_diff <- abs(diffs) * sqrt(v_siri)
    
    list(est = exp(lp_diff), lo = exp(lp_diff - 1.96 * se_diff), hi = exp(lp_diff + 1.96 * se_diff))
  }
  
  # Helper: Wald P-value
  wald_p_custom <- function(mod, terms_grep) {
    b <- mod$coef
    b <- b[!is.na(b)]
    idx <- grep(terms_grep, names(b))
    if(length(idx)==0) return(NA)
    
    b_sub <- b[idx]
    V_full <- as.matrix(mod$vcov)
    keep_names <- names(b)
    V_sub <- V_full[keep_names, keep_names, drop=FALSE]
    V_sub <- V_sub[idx, idx, drop=FALSE]
    
    stat <- tryCatch({
      as.numeric(t(b_sub) %*% qr.solve(V_sub, b_sub, tol = 1e-12))
    }, error = function(e) NA)
    
    if(is.na(stat)) return(NA)
    stats::pchisq(stat, df=length(b_sub), lower.tail=FALSE)
  }
  
  # --- 4. Core Calculation Logic ---
  
  calc_curve_mixed <- function(mod_int, mod_main, kind, endpoint) {
    regimens <- mod_int$xlevels$regimen_cat
    out_list <- list()
    
    # A. Global (Main Effects)
    if(kind == "AFT_HR") {
      res_main <- calc_main_hr(mod_main, x_grid, q2)
    } else {
      res_main <- calc_main_or(mod_main, x_grid, q2)
    }
    
    out_list[["Global"]] <- data.frame(
      regimen = "Global (Main Eff.)", 
      endpoint = endpoint,
      logsiri = x_grid,
      est = res_main$est, lo = res_main$lo, hi = res_main$hi
    )
    
    # B. Subgroups (Interaction)
    diam0 <- "Low"; ecog0 <- "0"; cacs0 <- "No"
    
    for (rp in regimens) {
      nd_ref <- ref_row(mod_int, q2, rp, diam0, ecog0, cacs0)
      ests <- numeric(length(x_grid))
      los <- numeric(length(x_grid))
      his <- numeric(length(x_grid))
      
      for (i in seq_along(x_grid)) {
        x <- x_grid[i]
        nd_x <- ref_row(mod_int, x, rp, diam0, ecog0, cacs0)
        cc <- lp_contrast_mod_w(mod_int, nd_ref, nd_x)
        rr <- ratio_from_delta(mod_int, cc$delta, cc$var_delta, kind)
        ests[i] <- rr[1]; los[i] <- rr[2]; his[i] <- rr[3]
      }
      out_list[[rp]] <- data.frame(
        regimen = rp, endpoint = endpoint, logsiri = x_grid, 
        est = ests, lo = los, hi = his
      )
    }
    do.call(rbind, out_list)
  }
  
  # --- 5. Generate Data ---
  df1 <- calc_curve_mixed(mod_pfs_int, mod_pfs_main, "AFT_HR", "PFS")
  df2 <- calc_curve_mixed(mod_os_int, mod_os_main, "AFT_HR", "OS")
  df3 <- calc_curve_mixed(mod_resp_int, mod_resp_main, "LOGIT_OR", "Response")
  
  df_curves <- rbind(df1, df2, df3)
  
  .drop_other <- trimws(as.character(df_curves$regimen)) == "Other"
  df_curves <- df_curves[!.drop_other, , drop = FALSE]
  
  df_curves$endpoint <- factor(df_curves$endpoint, levels = c("PFS", "OS", "Response"))
  
  reg_levels <- unique(as.character(df_curves$regimen))
  regs_only  <- setdiff(reg_levels, "Global (Main Eff.)")
  final_levels <- c(regs_only, "Global (Main Eff.)") 
  df_curves$regimen <- factor(df_curves$regimen, levels = final_levels)
  
  # --- 6. P-Values ---
  calc_pvals_mixed <- function(mod_int, mod_main, endpoint, regs) {
    out <- data.frame(regimen = regs, endpoint = endpoint, p_overall = NA, p_nl = NA)
    
    for(r in regs) {
      if(r == "Global (Main Eff.)") {
        out$p_overall[out$regimen==r] <- wald_p_custom(mod_main, "logsiri")
        if(length(grep("logsiri", names(mod_main$coef))) > 1) {
          nms <- grep("logsiri", names(mod_main$coef), value=TRUE)
          if(length(nms) > 1) {
            out$p_nl[out$regimen==r] <- wald_p_custom(mod_main, paste(nms[-1], collapse="|"))
          }
        }
      } else {
        term_r <- gsub("([\\^\\$\\.|\\(\\)\\[\\]\\*\\+\\?\\\\])", "\\\\\\1", r)
        pat <- paste0("logsiri.*", term_r, "|", term_r, ".*logsiri")
        if(length(grep(pat, names(mod_int$coef))) == 0) {
          pat <- "^logsiri" 
        }
        out$p_overall[out$regimen==r] <- wald_p_custom(mod_int, pat)
      }
    }
    return(out)
  }
  
  regs_u <- levels(df_curves$regimen)
  p1 <- calc_pvals_mixed(mod_pfs_int, mod_pfs_main, "PFS", regs_u)
  p2 <- calc_pvals_mixed(mod_os_int, mod_os_main, "OS", regs_u)
  p3 <- calc_pvals_mixed(mod_resp_int, mod_resp_main, "Response", regs_u)
  df_p_pan <- rbind(p1, p2, p3)
  
  fmt_p <- function(p) {
    p <- as.numeric(p)
    out <- character(length(p))
    out[is.na(p)] <- ""
    out[!is.na(p) & p < 0.001] <- "<0.001"
    idx <- !is.na(p) & p >= 0.001
    out[idx] <- sprintf("%.3f", p[idx])
    out
  }
  
  df_p_pan$lab <- ifelse(!is.na(df_p_pan$p_nl) & df_p_pan$p_nl < 0.05, 
                         paste0("p = ", fmt_p(df_p_pan$p_overall), "\nNon-lin p = ", fmt_p(df_p_pan$p_nl)),
                         paste0("p = ", fmt_p(df_p_pan$p_overall)))
  
  df_p_pan$lab[df_p_pan$lab == "p = "] <- ""
  df_p_pan$x <- -Inf; df_p_pan$y <- Inf
  
  # --- 7. Plotting ---
  pal_line <- c(PFS = "#1E5AA8", OS = "#35686C", Response = "#5A6C80")
  pal_fill <- pal_line
  
  lab_regimen <- function(x) {
    x0 <- as.character(x)
    x0[grepl("^gemcitabine\\s+mono$", trimws(x0), ignore.case = TRUE)] <- "Gemcitabine"
    x0
  }
  
  p <- ggplot2::ggplot(df_curves, ggplot2::aes(x = logsiri, y = est)) + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi, fill = endpoint), alpha = 0.18, color = NA) + 
    ggplot2::geom_line(ggplot2::aes(color = endpoint), linewidth = 0.95) + 
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey45", linewidth = 0.6) + 
    ggplot2::geom_vline(xintercept = q2, linetype = "dotted", color = "grey55", linewidth = 0.55) + 
    ggplot2::geom_text(data = df_p_pan, ggplot2::aes(x = x, y = y, label = lab), 
                       inherit.aes = FALSE, hjust = -0.05, vjust = 1.2, 
                       size = 3, color = "grey15", lineheight = 0.9) + 
    ggplot2::facet_grid(regimen ~ endpoint, labeller = ggplot2::labeller(regimen = lab_regimen)) + 
    ggplot2::scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3)) + 
    ggplot2::scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 4, 7)) + 
    ggplot2::coord_cartesian(ylim = c(0.1, 7), clip = "off") + 
    ggplot2::scale_color_manual(values = pal_line, guide = "none") + 
    ggplot2::scale_fill_manual(values = pal_fill, guide = "none") + 
    ggplot2::labs(x = "logSIRI", 
                  y = "HR / OR relative to median SIRI", 
                  title = "Effect of logSIRI: Global (Main Effects) vs Regimens", 
                  subtitle = "Bottom Row: Main Effects Models. Upper Rows: Interaction Models.") + 
    ggplot2::theme_classic(base_size = 14) + 
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey95", color = NA), 
                   strip.text = ggplot2::element_text(face = "bold", size = 11), 
                   axis.title = ggplot2::element_text(size = 13), 
                   axis.text = ggplot2::element_text(size = 11.5), 
                   plot.margin = ggplot2::margin(8, 8, 8, 8))
  
  return(p)
}