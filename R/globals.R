utils::globalVariables(c(
  # Variables generales y de gráficos
  "x", "y", "grp", "val", "label", "lab", "time", "lower", "upper", "surv",
  "est", "lo", "hi", "p_val", "panel_group", 
  
  # Nuevas variables de fig_2 (Publicación)
  "estimate", "var_label", "Outcome", "label_text",
  
  # Variables clínicas y demográficas
  "age", "edad", "sex", "smoking", "alcohol", "smoke_ordinal", "exercise",
  "familial_cancer", "steroids", 
  
  # Variables oncológicas y SIRI
  "SIRI", "logsiri", "SIRI_cat", "siri_high", 
  "pfs_time1l", "regimen_cat", "regimen_cat_clean", 
  "recist_plot", "measurable_disease", 
  "ecog_cat_3", "ecog_cat_table1", "endpoint",
  
  # Síntomas y CACS
  "asthenia", "anorexia", "weight_loss_bin", "cachexia", 
  "symp_tumorpain", "CACS_syndrome", "obstructive_jaundice", "vte_basal",
  
  # Metástasis y localización
  "head_pancreas", "body_tail", 
  "met_liver", "liver_met", "liver_burden",
  "met_lung", "lung_met", 
  "met_peritoneum", "peritoneum_met",
  "met_locoregional_ln"
))