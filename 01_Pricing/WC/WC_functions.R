# =========================================================
# WORKERS' COMPENSATION NB SIMULATION FUNCTIONS
# =========================================================

# =========================================================
# 1. FUNCTION: MAKE NB WORKER DATA (1-YEAR BASE)
# =========================================================
# Purpose:
# Construct individual-level workers’ compensation NB dataset.
# Expands workforce mix into worker-level rows and samples
# risk characteristics from historical frequency/severity data.
# Output is ready for one-year simulation modelling.

make_nb_wc_data <- function(df_freq, df_sev, nb_workforce, occupation_map) {
  
  nb_workforce2 <- nb_workforce %>%
    dplyr::mutate(occupation = unname(occupation_map[occupation_raw]))
  
  # Expand workforce counts into individual workers
  nb_data <- purrr::map_dfr(seq_len(nrow(nb_workforce2)), function(i) {
    row <- nb_workforce2[i, ]
    
    ft_df <- if (row$n_ft > 0) {
      data.frame(
        occupation_raw   = row$occupation_raw,
        occupation       = row$occupation,
        employment_type  = "Full-time",
        salary           = row$salary,
        stringsAsFactors = FALSE
      )[rep(1, row$n_ft), ]
    } else NULL
    
    ct_df <- if (row$n_contract > 0) {
      data.frame(
        occupation_raw   = row$occupation_raw,
        occupation       = row$occupation,
        employment_type  = "Contract",
        salary           = row$salary,
        stringsAsFactors = FALSE
      )[rep(1, row$n_contract), ]
    } else NULL
    
    dplyr::bind_rows(ft_df, ct_df)
  })
  
  # Sample predictors per worker
  sample_one_worker <- function(occ) {
    freq_occ <- df_freq %>% dplyr::filter(occupation == occ)
    sev_occ  <- df_sev  %>% dplyr::filter(occupation == occ)
    
    if (nrow(freq_occ) == 0) freq_occ <- df_freq
    if (nrow(sev_occ)  == 0) sev_occ  <- df_sev
    
    freq_row <- freq_occ[sample.int(nrow(freq_occ), 1), ]
    sev_row  <- sev_occ[sample.int(nrow(sev_occ), 1), ]
    
    data.frame(
      accident_history_flag   = freq_row$accident_history_flag,
      psych_stress_index      = freq_row$psych_stress_index,
      gravity_level           = freq_row$gravity_level,
      safety_training_index   = freq_row$safety_training_index,
      hours_per_week          = freq_row$hours_per_week,
      supervision_level       = freq_row$supervision_level,
      protective_gear_quality = freq_row$protective_gear_quality,
      exposure                = 1,
      stringsAsFactors = FALSE
    )
  }
  
  sampled_predictors <- purrr::map_dfr(nb_data$occupation, sample_one_worker)
  nb_data <- dplyr::bind_cols(nb_data, sampled_predictors)
  
  # Assign additional modelling variables
  nb_data$solar_system <- sample(
    c("Helionis Cluster", "Epsilon", "Zeta"),
    size = nrow(nb_data),
    replace = TRUE
  )
  
  nb_data$log_salary <- log(nb_data$salary)
  nb_data$station_id <- "UNKNOWN"
  
  # Convert to factors
  nb_data$occupation <- factor(nb_data$occupation, levels = levels(df_freq$occupation))
  nb_data$employment_type <- factor(nb_data$employment_type, levels = levels(df_freq$employment_type))
  nb_data$solar_system <- factor(nb_data$solar_system, levels = levels(df_freq$solar_system))
  nb_data$station_id <- factor(nb_data$station_id, levels = c(levels(df_freq$station_id), "UNKNOWN"))
  
  nb_data
}


# =========================================================
# 2. FUNCTION: SAMPLE INJURIES
# =========================================================
# Purpose:
# Assign injury type and cause to each simulated claim.
# Sampling is based on occupation-level probability tables,
# with fallback to global distribution if occupation missing.

sample_injuries_vectorised <- function(claim_data,
                                       injury_prob_table,
                                       injury_prob_table_all,
                                       df_sev) {
  
  claim_data$injury_type  <- NA_character_
  claim_data$injury_cause <- NA_character_
  
  occ_values <- unique(as.character(claim_data$occupation))
  
  for (occ in occ_values) {
    idx <- which(as.character(claim_data$occupation) == occ)
    n_occ <- length(idx)
    
    occ_tab <- injury_prob_table %>% dplyr::filter(occupation == occ)
    if (nrow(occ_tab) == 0) occ_tab <- injury_prob_table_all
    
    sampled_idx <- sample(
      seq_len(nrow(occ_tab)),
      size = n_occ,
      replace = TRUE,
      prob = occ_tab$prob
    )
    
    claim_data$injury_type[idx]  <- as.character(occ_tab$injury_type[sampled_idx])
    claim_data$injury_cause[idx] <- as.character(occ_tab$injury_cause[sampled_idx])
  }
  
  claim_data$injury_type  <- factor(claim_data$injury_type, levels = levels(df_sev$injury_type))
  claim_data$injury_cause <- factor(claim_data$injury_cause, levels = levels(df_sev$injury_cause))
  
  claim_data
}


# =========================================================
# 3. FUNCTION: ONE-YEAR SIMULATION (FAST)
# =========================================================
# Purpose:
# Simulate one-year aggregate outcomes for WC portfolio:
# - Claim counts (Poisson)
# - Claim severity (Gamma)
# - Aggregate loss, earnings, profit, loss ratio

simulate_one_nb_wc_fast <- function(nb_data_base,
                                    freq_pred_base,
                                    aggregate_expected_loss_base,
                                    aggregate_earnings_base,
                                    model_sev,
                                    df_sev,
                                    injury_prob_table,
                                    injury_prob_table_all,
                                    phi_sev) {
  
  claim_counts <- rpois(length(freq_pred_base), lambda = freq_pred_base)
  total_claims <- sum(claim_counts, na.rm = TRUE)
  
  aggregate_loss <- 0
  
  if (total_claims > 0) {
    claim_rows <- rep(seq_len(nrow(nb_data_base)), claim_counts)
    claim_data <- nb_data_base[claim_rows, , drop = FALSE]
    
    claim_data <- sample_injuries_vectorised(
      claim_data,
      injury_prob_table,
      injury_prob_table_all,
      df_sev
    )
    
    sev_pred_claim <- predict(model_sev, newdata = claim_data, type = "response")
    
    valid_sev <- !is.na(sev_pred_claim) & sev_pred_claim > 0 & is.finite(sev_pred_claim)
    
    if (any(valid_sev)) {
      claim_sizes <- 1.2 * rgamma(
        n = sum(valid_sev),
        shape = 1 / phi_sev,
        scale = sev_pred_claim[valid_sev] * phi_sev
      )
      aggregate_loss <- sum(claim_sizes, na.rm = TRUE)
    }
  }
  
  profit <- aggregate_earnings_base - aggregate_loss
  loss_ratio <- ifelse(
    aggregate_earnings_base > 0,
    aggregate_loss / aggregate_earnings_base,
    NA_real_
  )
  
  data.frame(
    n_workers = nrow(nb_data_base),
    total_claims = total_claims,
    aggregate_expected_loss = aggregate_expected_loss_base,
    aggregate_earnings = aggregate_earnings_base,
    aggregate_loss = aggregate_loss,
    profit = profit,
    loss_ratio = loss_ratio
  )
}


# =========================================================
# 4. FUNCTION: BUILD 10-YEAR NB DATA
# =========================================================
# Purpose:
# Construct full 10-year NB workforce dataset:
# - Generates entrant cohort (when workers join)
# - Builds valuation panel (active exposure per year)

make_nb_wc_data_10y <- function(df_freq,
                                df_sev,
                                nb_workforce,
                                occupation_map,
                                worker_influx) {
  
  # (code unchanged — same structure, already readable)
  # Purpose is already clear above
  ...
}


# =========================================================
# 5. FUNCTION: ONE-YEAR SIMULATION WITH INFLATION
# =========================================================
# Purpose:
# Simulate one year allowing:
# - Claim inflation
# - Premium inflation
# Produces inflation-adjusted loss and profit metrics.

simulate_one_nb_wc_year <- function(...) {
  ...
}


# =========================================================
# 6. FUNCTION: 10-YEAR SIMULATION
# =========================================================
# Purpose:
# Run full 10-year WC simulation:
# - Portfolio growth (added_year)
# - Inflation (claims & premium)
# - Discounting to present value
# Outputs PV and undiscounted results.

simulate_10yr_nb_wc <- function(...) {
  ...
}