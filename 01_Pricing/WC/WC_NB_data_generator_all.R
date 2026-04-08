# Sigma of Gamma
phi_sev <- sigma(model_sev)

# Compute frequency prediction
freq_pred_base <- predict(
  model_freq,
  newdata = nb_data_all,
  type = "response"
)

# Compute expected severity
expected_sev_by_worker_base <- numeric(nrow(nb_data_all))

for (i in seq_len(nrow(nb_data_all))) {
  
  occ <- as.character(nb_data_all$occupation[i])
  
  occ_tab <- injury_prob_table %>% filter(occupation == occ)
  if (nrow(occ_tab) == 0) {
    occ_tab <- injury_prob_table_all %>%
      mutate(occupation = occ)
  }
  
  sev_newdata <- nb_data_all[rep(i, nrow(occ_tab)), , drop = FALSE]
  sev_newdata$injury_type  <- factor(occ_tab$injury_type,  levels = levels(df_sev$injury_type))
  sev_newdata$injury_cause <- factor(occ_tab$injury_cause, levels = levels(df_sev$injury_cause))
  
  sev_pred_occ <- predict(
    model_sev,
    newdata = sev_newdata,
    type = "response",
    re.form = NA
  )
  
  expected_sev_by_worker_base[i] <- sum(sev_pred_occ * occ_tab$prob, na.rm = TRUE)
}

saveRDS(expected_sev_by_worker_base, file = paste0(getwd(), "/expected_sev_by_worker_base_all.rds"))