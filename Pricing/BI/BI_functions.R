library(dplyr)
library(tidyr)
library(purrr)

# =========================================================
# Function: make_nb_data_bi
#
# Purpose:
# Create policy-level new business data for 10 years by sampling
# from historical df_freq according to entry counts by year/system.
#
# Mapping:
# Helionis -> Helionis Cluster
# Bayesia   -> Epsilon
# Oryn      -> Zeta
# =========================================================
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

make_nb_data_bi_sep <- function(df_freq, nb_entry_table) {
  
  system_map <- c(
    "Helionis Cluster" = "Helionis Cluster",
    "Bayesia System"   = "Epsilon",
    "Oryn Delta"      = "Zeta"
  )
  
  nb_long <- nb_entry_table %>%
    rownames_to_column("system_raw") %>%
    pivot_longer(
      cols = starts_with("Yr"),
      names_to = "year",
      values_to = "n_new"
    ) %>%
    mutate(
      added_year   = as.integer(gsub("Yr", "", year)),
      solar_system = unname(system_map[system_raw])
    ) %>%
    dplyr::select(system_raw, solar_system, added_year, n_new)
  
  sampled_list <- map(seq_len(nrow(nb_long)), function(i) {
    
    row_i <- nb_long[i, ]
    n_i   <- row_i$n_new
    
    if (n_i == 0) return(NULL)
    
    pool <- df_freq %>%
      filter(solar_system == row_i$solar_system)
    
    if (nrow(pool) == 0) {
      stop(paste("No rows found in df_freq for solar_system =", row_i$solar_system))
    }
    
    tibble(
      solar_system         = row_i$solar_system,
      system_raw           = row_i$system_raw,
      added_year           = row_i$added_year,
      supply_chain_index   = sample(pool$supply_chain_index,   n_i, replace = TRUE),
      avg_crew_exp         = sample(pool$avg_crew_exp,         n_i, replace = TRUE),
      maintenance          = sample(pool$maintenance,          n_i, replace = TRUE),
      production_load      = sample(pool$production_load,      n_i, replace = TRUE),
      energy_backup_score  = sample(pool$energy_backup_score,  n_i, replace = TRUE),
      safety_compliance    = sample(pool$safety_compliance,    n_i, replace = TRUE),
      exposure             = 1,
      claim_count          = 0
    )
  })
  
  bind_rows(sampled_list)
}

nb_entry_table <- data.frame(
  row.names = c("Helionis Cluster", "Bayesia System", "Oryn Delta"),
  Yr1  = c(30, 15, 10),
  Yr2  = c(1,  0,  0),
  Yr3  = c(1,  1,  0),
  Yr4  = c(1,  0,  0),
  Yr5  = c(1,  1,  1),
  Yr6  = c(0,  0,  0),
  Yr7  = c(1,  1,  0),
  Yr8  = c(1,  0,  0),
  Yr9  = c(1,  1,  1),
  Yr10 = c(1,  0,  0)
)

library(dplyr)

# =========================================================
# One-year NB BI simulation
# =========================================================
simulate_one_nb_bi <- function(nb_data_base,
                               freq_pred_base,
                               nb_size,
                               mu,
                               sigma,
                               aggregate_expected_loss_base,
                               aggregate_earnings_base) {
  
  claim_counts <- rnbinom(
    n = length(freq_pred_base),
    size = nb_size,
    mu = freq_pred_base
  )
  
  total_claims <- sum(claim_counts, na.rm = TRUE)
  aggregate_loss <- 0
  
  if (total_claims > 0) {
    claim_sizes <- rlnorm(
      n = total_claims,
      meanlog = mu,
      sdlog   = sigma
    )
    
    aggregate_loss <- sum(claim_sizes, na.rm = TRUE)
  }
  
  profit <- aggregate_earnings_base - aggregate_loss
  loss_ratio <- ifelse(
    aggregate_earnings_base > 0,
    aggregate_loss / aggregate_earnings_base,
    NA_real_
  )
  
  data.frame(
    n_policies              = nrow(nb_data_base),
    total_claims            = total_claims,
    aggregate_expected_loss = aggregate_expected_loss_base,
    aggregate_earnings      = aggregate_earnings_base,
    aggregate_loss          = aggregate_loss,
    profit                  = profit,
    loss_ratio              = loss_ratio
  )
}

# =========================================================
# Multi-year NB BI simulation
# =========================================================
simulate_nyr_nb_bi <- function(nb_data_all,
                               freq_pred_all,
                               nb_size,
                               mu,
                               sigma,
                               expected_loss_by_policy_all,
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
    expected_loss_t_by_policy <- expected_loss_by_policy_all[idx_t]
    
    aggregate_expected_loss_base_t <- sum(expected_loss_t_by_policy, na.rm = TRUE)
    aggregate_earnings_base_t <- aggregate_expected_loss_base_t * (1 + loading)
    
    if (t == 1) {
      premium_inflation_factor_t <- 1
    } else {
      premium_inflation_factor_t <- prod(1 + inflation_vec[1:(t - 1)])
    }
    
    if (t == 1) {
      claim_inflation_factor_t <- exp(0.5 * inflation_vec[t])
    } else {
      claim_inflation_factor_t <- prod(1 + inflation_vec[1:(t - 1)]) * exp(0.5 * inflation_vec[t])
    }
    
    if (t == 1) {
      df_premium_t <- 1
      df_claim_t   <- 1
      # df_claim_t   <- 1 / ((1 + interest_vec[t])^0.5)
    } else {
      df_premium_t <- 1 / prod(1 + interest_vec[1:(t - 1)])
      df_claim_t <- 1 / prod(1 + interest_vec[1:(t - 1)])
      # df_claim_t   <- df_premium_t / ((1 + interest_vec[t])^0.5)
    }
    
    aggregate_expected_loss_t <- aggregate_expected_loss_base_t * claim_inflation_factor_t
    aggregate_earnings_t      <- aggregate_earnings_base_t * premium_inflation_factor_t
    
    claim_counts_t <- rnbinom(
      n = length(freq_pred_t),
      size = nb_size,
      mu = freq_pred_t
    )
    
    total_claims_t <- sum(claim_counts_t, na.rm = TRUE)
    aggregate_loss_t <- 0
    
    if (total_claims_t > 0) {
      claim_sizes_t <- rlnorm(
        n = total_claims_t,
        meanlog = mu + log(claim_inflation_factor_t),
        sdlog   = sigma
      )
      
      aggregate_loss_t <- sum(claim_sizes_t, na.rm = TRUE)
    }
    
    profit_t <- aggregate_earnings_t - aggregate_loss_t
    
    pv_loss_t    <- aggregate_loss_t * df_claim_t
    pv_premium_t <- aggregate_earnings_t * df_premium_t
    pv_profit_t  <- pv_premium_t - pv_loss_t
    
    pv_aggregate_loss   <- pv_aggregate_loss + pv_loss_t
    pv_aggregate_prem   <- pv_aggregate_prem + pv_premium_t
    pv_aggregate_profit <- pv_aggregate_profit + pv_profit_t
    
    yearly_results[[t]] <- data.frame(
      year = t,
      n_policies = nrow(nb_data_t),
      total_claims = total_claims_t,
      aggregate_expected_loss = aggregate_expected_loss_t,
      aggregate_earnings = aggregate_earnings_t,
      aggregate_loss = aggregate_loss_t,
      profit = profit_t,
      pv_loss = pv_loss_t,
      pv_premium = pv_premium_t,
      pv_profit = pv_profit_t,
      claim_inflation_factor = claim_inflation_factor_t,
      premium_inflation_factor = premium_inflation_factor_t,
      df_claim = df_claim_t,
      df_premium = df_premium_t
    )
  }
  
  yearly_results_df <- bind_rows(yearly_results)
  
  list(
    summary = data.frame(
      pv_aggregate_loss = pv_aggregate_loss,
      pv_aggregate_premium = pv_aggregate_prem,
      pv_aggregate_profit = pv_aggregate_profit,
      undiscounted_aggregate_loss = sum(yearly_results_df$aggregate_loss, na.rm = TRUE),
      undiscounted_aggregate_profit = sum(yearly_results_df$profit, na.rm = TRUE)
    ),
    yearly = yearly_results_df
  )
}

# =========================================================
# Wrapper function
# =========================================================
run_nb_bi_simulation <- function(nb_data_bi,
                                 model_freq,
                                 mu,
                                 sigma,
                                 n_sim = 10000,
                                 n_years = 1,
                                 loading = 0.05,
                                 inflation_vec = rep(0.025, n_years),
                                 interest_vec = rep(0.018, n_years)) {
  
  nb_size <- model_freq$theta
  
  if (n_years == 1) {
    
    nb_data_base <- nb_data_bi %>%
      dplyr::filter(added_year == 1)
    
    freq_pred_base <- predict(
      model_freq,
      newdata = nb_data_base,
      type = "response"
    )
    
    expected_sev <- exp(mu + 0.5 * sigma^2)
    expected_loss_by_policy_base <- freq_pred_base * expected_sev
    
    aggregate_expected_loss_base <- sum(expected_loss_by_policy_base, na.rm = TRUE)
    aggregate_earnings_base <- aggregate_expected_loss_base * (1 + loading)
    
    sim_list <- vector("list", n_sim)
    
    for (sim in seq_len(n_sim)) {
      sim_list[[sim]] <- simulate_one_nb_bi(
        nb_data_base = nb_data_base,
        freq_pred_base = freq_pred_base,
        nb_size = nb_size,
        mu = mu,
        sigma = sigma,
        aggregate_expected_loss_base = aggregate_expected_loss_base,
        aggregate_earnings_base = aggregate_earnings_base
      )
    }
    
    return(bind_rows(sim_list))
    
  } else {
    
    nb_data_all <- nb_data_bi
    
    freq_pred_all <- predict(
      model_freq,
      newdata = nb_data_all,
      type = "response"
    )
    
    expected_sev <- exp(mu + 0.5 * sigma^2)
    expected_loss_by_policy_all <- freq_pred_all * expected_sev
    
    sim_list <- vector("list", n_sim)
    
    for (sim in seq_len(n_sim)) {
      sim_out <- simulate_nyr_nb_bi(
        nb_data_all = nb_data_all,
        freq_pred_all = freq_pred_all,
        nb_size = nb_size,
        mu = mu,
        sigma = sigma,
        expected_loss_by_policy_all = expected_loss_by_policy_all,
        loading = loading,
        inflation_vec = inflation_vec,
        interest_vec = interest_vec
      )
      
      sim_list[[sim]] <- sim_out$summary
    }
    
    return(bind_rows(sim_list))
  }
}
