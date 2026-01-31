#' Internal Rubin Model Utilities
#'
#' Helper functions for Wald tests, linear contrasts, and reference grids.
#' These functions support the generation of figures and tables for the Rubin models.
#'
#' @keywords internal
#' @name utils_rubin
NULL

# ==============================================================================
# 1. IMPORTS (CRITICAL FOR ns() ERRORS)
# ==============================================================================
#' @importFrom MASS ginv
#' @importFrom splines ns
#' @importFrom stats pchisq model.frame model.matrix delete.response quantile median
NULL

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================

# --- Wald Test (Blockwise) ---
anova_wald_internal <- function(mod) {
  if (is.null(mod$coef) || is.null(mod$vcov) || is.null(mod$terms)) return(NULL)
  
  b0 <- mod$coef
  V0 <- as.matrix(mod$vcov)
  keep <- !is.na(b0)
  b <- b0[keep]
  V <- V0[keep, keep, drop = FALSE]
  nms <- names(b)
  term_labels <- attr(mod$terms, "term.labels")
  
  # Helper: Safe Inverse using Generalized Inverse if singular
  inv_sym <- function(M) {
    tryCatch(solve(M), error = function(e) MASS::ginv(M))
  }
  
  # Helper: Regex to map terms to coefficients
  coef_for_term <- function(term) {
    if (term == "diam_low") grep("^diam_low", nms, value = TRUE)
    else if (grepl("^ns\\(", term)) grep("^ns\\(logsiri", nms, value = TRUE)
    else if (term == "logsiri") grep("^logsiri$", nms, value = TRUE)
    else if (term == "regimen_cat") { 
      idx <- grep("^regimen_cat", nms, value = TRUE)
      idx[!grepl(":", idx)] # Exclude interactions
    }
    else if (term == "ecog_cat_3") grep("^ecog_cat_3", nms, value = TRUE)
    else if (term == "CACS") grep("^CACS", nms, value = TRUE)
    else if (grepl(":", term)) grep(":", nms, value = TRUE)
    else grep(paste0("^", term), nms, value = TRUE)
  }
  
  out <- lapply(term_labels, function(term) {
    idx <- coef_for_term(term)
    idx <- idx[idx %in% names(b)]
    if (length(idx) == 0) return(data.frame(term=term, df=0, Chisq=NA, p=NA))
    
    bb <- as.numeric(b[idx])
    VV <- V[idx, idx, drop=FALSE]
    W <- as.numeric(t(bb) %*% inv_sym(VV) %*% bb)
    p <- stats::pchisq(W, df=length(bb), lower.tail=FALSE)
    data.frame(term=term, df=length(bb), Chisq=W, p=p)
  })
  
  res <- do.call(rbind, out)
  res$p_fmt <- ifelse(res$p < 0.001, "<0.001", sprintf("%.3f", res$p))
  return(res)
}

# --- Linear Contrasts (Delta Method) ---
lp_contrast_mod_w <- function(mod, nd_a, nd_b) {
  b0 <- mod$coef
  V0 <- as.matrix(mod$vcov)
  keep <- !is.na(b0)
  b <- b0[keep]
  V <- V0[keep, keep, drop = FALSE]
  
  tt <- stats::delete.response(mod$terms)
  
  # --- CRITICAL FIX: REPAIR ENVIRONMENT ---
  # Ensures 'ns' function is found within the package environment
  # This fixes the "could not find function 'ns'" error
  environment(tt) <- environment() 
  # ----------------------------------------
  
  mf_a <- stats::model.frame(tt, data = nd_a, xlev = mod$xlevels)
  mf_b <- stats::model.frame(tt, data = nd_b, xlev = mod$xlevels)
  Xa <- stats::model.matrix(tt, data = mf_a)
  Xb <- stats::model.matrix(tt, data = mf_b)
  
  common <- intersect(names(b), colnames(Xa))
  if (length(common) == 0) stop("Error: No coefficients match. Check factor levels.")
  
  w <- as.numeric(Xb[1, common] - Xa[1, common])
  names(w) <- common
  delta <- sum(w * b[common])
  var_delta <- as.numeric(t(w) %*% V[common, common] %*% w)
  
  list(w=w, delta=delta, var_delta=var_delta, b=b[common], V=V[common, common])
}

# --- Ratio Helper (HR/OR) ---
ratio_from_delta <- function(mod, delta, var_delta, kind) {
  if (kind == "AFT_HR") {
    sc <- mod$scale
    log_eff <- -delta / sc
    se <- sqrt(var_delta) / sc
  } else {
    log_eff <- delta
    se <- sqrt(var_delta)
  }
  est <- exp(log_eff)
  c(est=est, lo=exp(log_eff - 1.96*se), hi=exp(log_eff + 1.96*se))
}

# --- Reference Row Builder ---
ref_row <- function(mod, logsiri_value, regimen, diam="Low", ecog="0", cacs="No") {
  # Robust level access
  lv_diam <- if(!is.null(mod$xlevels$diam_low)) mod$xlevels$diam_low else c("Low", "High")
  lv_reg  <- if(!is.null(mod$xlevels$regimen_cat)) mod$xlevels$regimen_cat else c("FOLFIRINOX", "Gem-Abraxane", "Gemcitabine mono", "Other")
  lv_ecog <- if(!is.null(mod$xlevels$ecog_cat_3)) mod$xlevels$ecog_cat_3 else c("0", "1", "2+")
  lv_cacs <- if(!is.null(mod$xlevels$CACS)) mod$xlevels$CACS else c("No", "Yes")
  
  data.frame(
    logsiri = as.numeric(logsiri_value),
    diam_low = factor(diam, levels = lv_diam),
    regimen_cat = factor(regimen, levels = lv_reg),
    ecog_cat_3  = factor(ecog, levels = lv_ecog),
    CACS        = factor(cacs, levels = lv_cacs)
  )
}