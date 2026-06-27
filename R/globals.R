utils::globalVariables(c(
  # General and plotting variables
  "x", "y", "grp", "val", "label", "lab", "time", "lower", "upper", "surv",
  "est", "lo", "hi", "p_val", "panel_group", 
  
  # New variables for fig_2 (publication)
  "estimate", "var_label", "Outcome", "label_text",
  
  # Clinical and demographic variables
  "age", "edad", "sex", "smoking", "alcohol", "smoke_ordinal", "exercise",
  "familial_cancer", "steroids", 
  
  # Oncologic and SIRI variables
  "SIRI", "logsiri", "SIRI_cat", "siri_high", 
  "pfs_time1l", "regimen_cat", "regimen_cat_clean", 
  "recist_plot", "measurable_disease", 
  "ecog_cat_3", "ecog_cat_table1", "endpoint",
  
  # Symptoms and CACS
  "asthenia", "anorexia", "weight_loss_bin", "cachexia", 
  "symp_tumorpain", "CACS_syndrome", "obstructive_jaundice", "vte_basal",
  
  # Metastasis and location
  "head_pancreas", "body_tail", 
  "met_liver", "liver_met", "liver_burden",
  "met_lung", "lung_met", 
  "met_peritoneum", "peritoneum_met",
  "met_locoregional_ln",
  
  # data
  "pantheia_data"
))









