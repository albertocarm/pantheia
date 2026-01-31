#' Generate Baseline Patient Characteristics Table (Table 1)
#'
#' Reproduces Table 1. By default, it uses the internal package data (.rda) which preserves
#' factor levels (ordering). It can also accept a path to a CSV file.
#'
#' @param data A dataframe or a file path to a CSV. Defaults to \code{pantheia::pantheia_data}.
#' @return A gtsummary object.
#' @import dplyr
#' @import gtsummary
#' @importFrom utils read.csv
#' @importFrom dplyr %>%
#' @export
table1 <- function(data = pantheia::pantheia_data) {
  
  # 1. Handle file path input (if user provides a CSV path)
  if (is.character(data) && length(data) == 1) {
    if (!file.exists(data)) stop("The specified CSV file does not exist.")
    message("Reading data from external CSV... (Note: Factor levels/ordering might be lost)")
    data <- utils::read.csv(data)
  }
  
  if (!requireNamespace("gtsummary", quietly = TRUE)) {
    stop("Package 'gtsummary' is required.")
  }
  
  # 2. Ensure types and levels (critical if data comes from CSV)
  if("ecog_cat_table1" %in% names(data)){
    data$ecog_cat_table1 <- factor(data$ecog_cat_table1, levels = c("0", "1", "2", ">=3"))
  }
  
  if("smoking" %in% names(data)){
    data$smoking <- data$smoking
  }
  
  if("siri_high" %in% names(data)){
    data$siri_high <- factor(data$siri_high, levels = c("No", "Yes"))
  }
  
  # 3. Column validation
  # Ensure critical variables exist before selection
  vars_needed <- c("regimen_cat", "CACS_syndrome")
  missing <- setdiff(vars_needed, names(data))
  
  if(length(missing) > 0) {
    warning(paste("Missing variables in data:", paste(missing, collapse=", ")))
  }
  
  # 4. Generate table
  t1 <- data %>%
    dplyr::select(
      age, sex, ecog_cat_table1, smoking, alcohol, steroids, familial_cancer,
      asthenia, anorexia, weight_loss_bin, cachexia, 
      CACS_syndrome, 
      head_pancreas, body_tail, measurable_disease,
      met_liver, met_peritoneum, met_locoregional_ln, met_lung,
      siri_high, regimen_cat
    ) %>%
    gtsummary::tbl_summary(
      statistic = list(
        gtsummary::all_continuous() ~ "{mean} ({min}-{max})",
        gtsummary::all_categorical() ~ "{n} ({p}%)"
      ),
      label = list(
        age ~ "Age, years",
        sex ~ "Sex",
        ecog_cat_table1 ~ "ECOG performance status",
        smoking ~ "Smoking status",
        alcohol ~ "Alcohol consumption",
        steroids ~ "Prolonged steroid use",
        familial_cancer ~ "Familial cancer syndrome",
        asthenia ~ "Asthenia",
        anorexia ~ "Anorexia",
        weight_loss_bin ~ "Weight loss >5%",
        cachexia ~ "Cachexia",
        CACS_syndrome ~ "Cancer Anorexia-Cachexia Syndrome",
        head_pancreas ~ "Head of the pancreas",
        body_tail ~ "Body/Tail of the pancreas",
        measurable_disease ~ "Measurable disease",
        met_liver ~ "Liver metastasis",
        met_peritoneum ~ "Peritoneum metastasis",
        met_locoregional_ln ~ "Locoregional LN metastasis",
        met_lung ~ "Lung metastasis",
        siri_high ~ "Elevated SIRI (>2.3)",
        regimen_cat ~ "First-line regimen"
      ),
      missing = "no",
      digits = list(
        gtsummary::all_continuous() ~ 1, 
        gtsummary::all_categorical() ~ c(0, 1)
      )
    ) %>%
    gtsummary::bold_labels() %>%
    gtsummary::italicize_levels() %>%
    gtsummary::modify_caption("**Table 1. Baseline Patient Characteristics**")
  
  return(t1)
}