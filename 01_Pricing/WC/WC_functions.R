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
# 3. FUNCTION: ONE-YEAR SIMULATION
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
  
  nb_workforce2 <- nb_workforce %>%
    dplyr::mutate(occupation = unname(occupation_map[occupation_raw]))
  
  template_ft <- nb_workforce2 %>%
    dplyr::filter(n_ft > 0) %>%
    dplyr::transmute(
      occupation_raw,
      occupation,
      employment_type = "Full-time",
      salary,
      weight = n_ft
    )
  
  template_ct <- nb_workforce2 %>%
    dplyr::filter(n_contract > 0) %>%
    dplyr::transmute(
      occupation_raw,
      occupation,
      employment_type = "Contract",
      salary,
      weight = n_contract
    )
  
  worker_templates <- dplyr::bind_rows(template_ft, template_ct)
  
  influx_long <- worker_influx %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("yr"),
      names_to = "year_label",
      values_to = "n_new"
    ) %>%
    dplyr::mutate(
      added_year = as.integer(gsub("yr", "", year_label))
    ) %>%
    dplyr::select(occupation, added_year, n_new)
  
  sample_worker_templates <- function(occ, n) {
    occ_templates <- worker_templates %>%
      dplyr::filter(occupation == occ)
    
    if (nrow(occ_templates) == 0) {
      stop(paste("No worker template found for occupation:", occ))
    }
    
    sampled_idx <- sample.int(
      n = nrow(occ_templates),
      size = n,
      replace = TRUE,
      prob = occ_templates$weight
    )
    
    occ_templates[sampled_idx, c("occupation_raw", "occupation", "employment_type", "salary")]
  }
  
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
  
  entrants_list <- purrr::pmap_dfr(
    influx_long,
    function(occupation, added_year, n_new) {
      if (n_new <= 0) return(NULL)
      
      worker_info <- sample_worker_templates(occupation, n_new)
      predictors  <- purrr::map_dfr(rep(occupation, n_new), sample_one_worker)
      
      dplyr::bind_cols(worker_info, predictors) %>%
        dplyr::mutate(added_year = added_year)
    }
  )
  
  entrants <- entrants_list %>%
    dplyr::mutate(
      worker_id = paste0("W", seq_len(dplyr::n())),
      solar_system = sample(
        c("Helionis Cluster", "Epsilon", "Helionis Cluster"),
        size = dplyr::n(),
        replace = TRUE
      ),
      log_salary = log(salary),
      station_id = "UNKNOWN"
    ) %>%
    dplyr::select(
      worker_id,
      added_year,
      occupation_raw,
      occupation,
      employment_type,
      salary,
      log_salary,
      solar_system,
      station_id,
      accident_history_flag,
      psych_stress_index,
      gravity_level,
      safety_training_index,
      hours_per_week,
      supervision_level,
      protective_gear_quality,
      exposure
    )
  
  entrants$occupation <- factor(entrants$occupation, levels = levels(df_freq$occupation))
  entrants$employment_type <- factor(entrants$employment_type, levels = levels(df_freq$employment_type))
  entrants$solar_system <- factor(entrants$solar_system, levels = levels(df_freq$solar_system))
  entrants$station_id <- factor(entrants$station_id, levels = c(levels(df_freq$station_id), "UNKNOWN"))
  
  nb_panel <- tidyr::crossing(
    entrants,
    valuation_year = 1:10
  ) %>%
    dplyr::filter(valuation_year >= added_year) %>%
    dplyr::mutate(
      policy_year = valuation_year - added_year + 1
    )
  
  list(
    entrants = entrants,
    panel = nb_panel
  )
}


# =========================================================
# 5. FUNCTION: ONE-YEAR SIMULATION WITH INFLATION
# =========================================================
# Purpose:
# Simulate one year allowing:
# - Claim inflation
# - Premium inflation
# Produces inflation-adjusted loss and profit metrics.

simulate_one_nb_wc_year <- function(nb_data_base,
                                    freq_pred_base,
                                    aggregate_expected_loss_base,
                                    aggregate_earnings_base,
                                    model_sev,
                                    df_sev,
                                    injury_prob_table,
                                    injury_prob_table_all,
                                    phi_sev,
                                    claim_inflation_factor = 1,
                                    premium_inflation_factor = 1) {
  
  total_workers <- nrow(nb_data_base)
  
  aggregate_earnings_t <- aggregate_earnings_base * premium_inflation_factor
  aggregate_expected_loss_t <- aggregate_expected_loss_base * claim_inflation_factor
  
  claim_counts <- rpois(length(freq_pred_base), lambda = freq_pred_base)
  total_claims <- sum(claim_counts, na.rm = TRUE)
  
  aggregate_loss_t <- 0
  
  if (total_claims > 0) {
    claim_rows <- rep(seq_len(nrow(nb_data_base)), claim_counts)
    claim_data <- nb_data_base[claim_rows, , drop = FALSE]
    
    claim_data <- sample_injuries_vectorised(
      claim_data = claim_data,
      injury_prob_table = injury_prob_table,
      injury_prob_table_all = injury_prob_table_all,
      df_sev = df_sev
    )
    
    sev_pred_claim <- predict(
      model_sev,
      newdata = claim_data,
      type = "response"
    )
    
    sev_pred_claim_inflated <- sev_pred_claim * claim_inflation_factor
    
    valid_sev <- !is.na(sev_pred_claim_inflated) &
      sev_pred_claim_inflated > 0 &
      is.finite(sev_pred_claim_inflated)
    
    if (any(valid_sev)) {
      claim_sizes <- rgamma(
        n = sum(valid_sev),
        shape = 1 / phi_sev,
        scale = sev_pred_claim_inflated[valid_sev] * phi_sev
      )
      aggregate_loss_t <- sum(claim_sizes, na.rm = TRUE)
    }
  }
  
  profit_t <- aggregate_earnings_t - aggregate_loss_t
  loss_ratio_t <- ifelse(
    aggregate_earnings_t > 0,
    aggregate_loss_t / aggregate_earnings_t,
    NA_real_
  )
  
  data.frame(
    n_workers               = total_workers,
    total_claims            = total_claims,
    aggregate_expected_loss = aggregate_expected_loss_t,
    aggregate_earnings      = aggregate_earnings_t,
    aggregate_loss          = aggregate_loss_t,
    profit                  = profit_t,
    loss_ratio              = loss_ratio_t
  )
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

simulate_10yr_nb_wc <- function(nb_data_all,
                                freq_pred_all,
                                expected_loss_by_worker_all,
                                model_sev,
                                df_sev,
                                injury_prob_table,
                                injury_prob_table_all,
                                phi_sev,
                                loading,
                                inflation_vec,
                                interest_vec) {
  
  n_years <- length(inflation_vec)
  stopifnot(length(interest_vec) == n_years)
  
  pv_aggregate_loss   <- 0
  pv_aggregate_profit <- 0
  pv_aggregate_prem   <- 0
  
  yearly_results <- vector("list", n_years)
  
  for (t in seq_len(n_years)) {
    
    idx_t <- which(nb_data_all$added_year <= t)
    nb_data_t <- nb_data_all[idx_t, , drop = FALSE]
    freq_pred_t <- freq_pred_all[idx_t]
    expected_loss_by_worker_t <- expected_loss_by_worker_all[idx_t]
    
    aggregate_expected_loss_base_t <- sum(expected_loss_by_worker_t, na.rm = TRUE)
    aggregate_earnings_base_t <- aggregate_expected_loss_base_t * (1 + loading)
    
    if (t == 1) {
      premium_inflation_factor_t <- 1
    } else {
      premium_inflation_factor_t <- prod(1 + inflation_vec[1:(t - 1)])
    }
    
    if (t == 1) {
      claim_inflation_factor_t <- 1
    } else {
      claim_inflation_factor_t <- prod(1 + inflation_vec[1:(t - 1)])
    }
    
    if (t == 1) {
      df_premium_t <- 1
      df_claim_t <-  1
    } else {
      df_premium_t <- 1 / prod(1 + interest_vec[1:(t - 1)])
      df_claim_t <- 1 / prod(1 + interest_vec[1:(t - 1)])
    }
    
    sim_t <- simulate_one_nb_wc_year(
      nb_data_base = nb_data_t,
      freq_pred_base = freq_pred_t,
      aggregate_expected_loss_base = aggregate_expected_loss_base_t,
      aggregate_earnings_base = aggregate_earnings_base_t,
      model_sev = model_sev,
      df_sev = df_sev,
      injury_prob_table = injury_prob_table,
      injury_prob_table_all = injury_prob_table_all,
      phi_sev = phi_sev,
      claim_inflation_factor = claim_inflation_factor_t,
      premium_inflation_factor = premium_inflation_factor_t
    )
    
    pv_loss_t <- sim_t$aggregate_loss * df_claim_t
    pv_premium_t <- sim_t$aggregate_earnings * df_premium_t
    pv_profit_t <- pv_premium_t - pv_loss_t
    
    pv_aggregate_loss   <- pv_aggregate_loss + pv_loss_t
    pv_aggregate_prem   <- pv_aggregate_prem + pv_premium_t
    pv_aggregate_profit <- pv_aggregate_profit + pv_profit_t
    
    yearly_results[[t]] <- data.frame(
      year = t,
      n_workers = sim_t$n_workers,
      total_claims = sim_t$total_claims,
      aggregate_expected_loss = sim_t$aggregate_expected_loss,
      aggregate_earnings = sim_t$aggregate_earnings,
      aggregate_loss = sim_t$aggregate_loss,
      profit = sim_t$profit,
      pv_loss = pv_loss_t,
      pv_premium = pv_premium_t,
      pv_profit = pv_profit_t,
      claim_inflation_factor = claim_inflation_factor_t,
      premium_inflation_factor = premium_inflation_factor_t,
      df_claim = df_claim_t,
      df_premium = df_premium_t
    )
  }
  
  yearly_results_df <- dplyr::bind_rows(yearly_results)
  
  data.frame(
    pv_aggregate_loss = pv_aggregate_loss,
    pv_aggregate_premium = pv_aggregate_prem,
    pv_aggregate_profit = pv_aggregate_profit,
    undiscounted_aggregate_loss = sum(yearly_results_df$aggregate_loss, na.rm = TRUE),
    undiscounted_aggregate_profit = sum(yearly_results_df$profit, na.rm = TRUE)
  )
}
