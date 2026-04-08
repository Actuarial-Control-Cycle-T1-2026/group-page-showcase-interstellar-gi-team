##### This code is to generate freq and sev tables of BI for GLM Modelling ######
# Input tables: bi_claim_freq, bi_claim_sev
# Output tables: wc_freq_model, wc_sev_model

# Import Libraries and Tables
library(readr)
library(dplyr)
library(tidyr)

# Set Paths
intermediate_path <- paste0(getwd(), '/00_DataPrep/IntermediateOutput/')
output_path <- paste0(getwd(), '/00_DataPrep/OutputData/')

wc_claim_freq    <- read_csv(paste0(intermediate_path,"wc_claim_freq.csv"))
wc_claim_sev    <- read_csv(paste0(intermediate_path,"wc_claim_sev.csv"))

##### FREQUENCY TABLE PIPELINE #####
# Cut suffix from character columns
wc_claim_freq <- wc_claim_freq %>% mutate(across(where(is.character),~ sub("_.*", "", .x)))
wc_claim_sev <- wc_claim_sev %>% mutate(across(where(is.character), ~ sub("_.*", "", .x)))

# Extract rows with NA policynumber in sev table
sev_no_policy <- wc_claim_sev %>% filter(is.na(policy_id)) #1 row with worker_id == W-110840 -> policy_id == WC-ZET-00830 in freq table

# Make change in sev table
wc_claim_sev <- wc_claim_sev %>% mutate(
  policy_id = if_else(
      is.na(policy_id) & worker_id == "W-110840",
      "WC-ZET-00830",
      policy_id
    ))

# Extract rows with NA workerid in sev table
sev_no_worker <- wc_claim_sev %>% filter(is.na(worker_id)) #1 row with policy_id == WC-EPS-00702 -> worker_id == W-81573 in freq table

# Make change in sev table
wc_claim_sev <- wc_claim_sev %>% mutate(
  worker_id = if_else(
    is.na(worker_id) & policy_id == "WC-EPS-00702",
    "W-81573",
    worker_id
  ))

# Select overlap cols for claim_sev right table
common_cols <- intersect(names(wc_claim_sev), names(wc_claim_freq))
wc_claim_sev_right <- wc_claim_sev %>%
  dplyr::select(common_cols)

# Fill NA for out-of-range values
wc_claim_sev_right <- wc_claim_sev_right %>%
  mutate(
    experience_yrs = if_else(experience_yrs >= 0.2 & experience_yrs <= 40, experience_yrs, NA_real_),
    exposure = if_else( exposure >= 0 & exposure <= 1, exposure, NA_real_),
    accident_history_flag = if_else(accident_history_flag %in% 0:1, accident_history_flag, NA_real_),
    psych_stress_index = if_else(psych_stress_index %in% 1:5, psych_stress_index, NA_real_),
    hours_per_week = if_else(hours_per_week %in% c(20, 25, 30, 35, 40), hours_per_week, NA_real_),
    supervision_level = if_else( supervision_level >= 0 & supervision_level <= 1, supervision_level, NA_real_),
    gravity_level = if_else( gravity_level >= 0.75 & gravity_level <= 1.5, gravity_level, NA_real_),
    safety_training_index = if_else(safety_training_index %in% 1:5, safety_training_index, NA_real_),
    protective_gear_quality = if_else(protective_gear_quality %in% 1:5, protective_gear_quality, NA_real_),
    base_salary = if_else( base_salary >= 20000 & base_salary <= 130000, base_salary, NA_real_)
  )

# CHECK 1: Any policy_id with worker id with different values?
check <- wc_claim_sev_right %>%
  group_by(policy_id, worker_id) %>%
  summarise(
    n_station_id              = n_distinct(station_id, na.rm = TRUE),
    n_solar_system            = n_distinct(solar_system, na.rm = TRUE),
    n_experience_yrs          = n_distinct(experience_yrs, na.rm = TRUE),
    n_exposure                = n_distinct(exposure, na.rm = TRUE),
    n_accident_history_flag   = n_distinct(accident_history_flag, na.rm = TRUE),
    n_psych_stress_index      = n_distinct(psych_stress_index, na.rm = TRUE),
    n_hours_per_week          = n_distinct(hours_per_week, na.rm = TRUE),
    n_supervision_level       = n_distinct(supervision_level, na.rm = TRUE),
    n_gravity_level           = n_distinct(gravity_level, na.rm = TRUE),
    n_safety_training_index   = n_distinct(safety_training_index, na.rm = TRUE),
    n_protective_gear_quality = n_distinct(protective_gear_quality, na.rm = TRUE),
    n_base_salary             = n_distinct(base_salary, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  filter(
    if_any(starts_with("n_"), ~ . > 1)
  ) # No combination with different values



# Group sev_right table by policy_id and worker_id
wc_claim_sev_policy <- wc_claim_sev_right %>%
  group_by(policy_id, worker_id) %>%
  summarise(
    claim_count = n(),
    
    station_id       = first(na.omit(station_id)),
    solar_system     = first(na.omit(solar_system)),
    occupation       = first(na.omit(occupation)),
    employment_type  = first(na.omit(employment_type)),
    
    exposure = if (all(is.na(exposure))) NA_real_ else first(na.omit(exposure)),
    
    experience_yrs = if (all(is.na(experience_yrs))) NA_real_ else first(na.omit(experience_yrs)),
    accident_history_flag = if (all(is.na(accident_history_flag))) NA_real_ else first(na.omit(accident_history_flag)),
    psych_stress_index = if (all(is.na(psych_stress_index))) NA_real_ else first(na.omit(psych_stress_index)),
    hours_per_week = if (all(is.na(hours_per_week))) NA_real_ else first(na.omit(hours_per_week)),
    supervision_level = if (all(is.na(supervision_level))) NA_real_ else first(na.omit(supervision_level)),
    gravity_level = if (all(is.na(gravity_level))) NA_real_ else first(na.omit(gravity_level)),
    safety_training_index = if (all(is.na(safety_training_index))) NA_real_ else first(na.omit(safety_training_index)),
    protective_gear_quality = if (all(is.na(protective_gear_quality))) NA_real_ else first(na.omit(protective_gear_quality)),
    base_salary = if (all(is.na(base_salary))) NA_real_ else first(na.omit(base_salary)),
    
    .groups = "drop"
  )

# Left Join on freq table
wc_freq_model <- wc_claim_freq %>%
  mutate(
    experience_yrs = if_else(experience_yrs >= 0.2 & experience_yrs <= 40, experience_yrs, NA_real_),
    exposure = if_else(exposure >= 0 & exposure <= 1, exposure, NA_real_),
    accident_history_flag = if_else(accident_history_flag %in% 0:1, accident_history_flag, NA_real_),
    psych_stress_index = if_else(psych_stress_index %in% 1:5, psych_stress_index, NA_real_),
    hours_per_week = if_else(hours_per_week %in% c(20, 25, 30, 35, 40), hours_per_week, NA_real_),
    supervision_level = if_else(supervision_level >= 0 & supervision_level <= 1, supervision_level, NA_real_),
    gravity_level = if_else(gravity_level >= 0.75 & gravity_level <= 1.5, gravity_level, NA_real_),
    safety_training_index = if_else(safety_training_index %in% 1:5, safety_training_index, NA_real_),
    protective_gear_quality = if_else(protective_gear_quality %in% 1:5, protective_gear_quality, NA_real_),
    base_salary = if_else(base_salary >= 20000 & base_salary <= 130000, base_salary, NA_real_),
    claim_count = if_else(claim_count %in% 1:3, claim_count, NA_real_),
  ) %>%
  left_join(
    wc_claim_sev_policy,
    by = c("policy_id", "worker_id"),
    suffix = c("_freq", "_sev")
  ) %>%
  mutate(
    station_id     = coalesce(station_id_freq, station_id_sev),
    solar_system   = coalesce(solar_system_freq, solar_system_sev),
    occupation     = coalesce(occupation_freq, occupation_sev),
    employment_type= coalesce(employment_type_freq, employment_type_sev),
    
    experience_yrs          = coalesce(experience_yrs_freq, experience_yrs_sev),
    exposure                = coalesce(exposure_freq, exposure_sev),
    accident_history_flag   = coalesce(accident_history_flag_freq, accident_history_flag_sev),
    psych_stress_index      = coalesce(psych_stress_index_freq, psych_stress_index_sev),
    hours_per_week          = coalesce(hours_per_week_freq, hours_per_week_sev),
    supervision_level       = coalesce(supervision_level_freq, supervision_level_sev),
    gravity_level           = coalesce(gravity_level_freq, gravity_level_sev),
    safety_training_index   = coalesce(safety_training_index_freq, safety_training_index_sev),
    protective_gear_quality = coalesce(protective_gear_quality_freq, protective_gear_quality_sev),
    base_salary             = coalesce(base_salary_freq, base_salary_sev),
    claim_count          = coalesce(claim_count_freq, claim_count_sev)
  ) %>%
  dplyr::select(-ends_with("_freq"), -ends_with("_sev"))

# Set claim_count as 0 if there is no claim history
wc_freq_model <- wc_freq_model %>%
  mutate(claim_count = coalesce(claim_count, 0))

##### SEVERITY TABLE PIPELINE #####
# Import Tables
wc_claim_freq    <- read_csv(paste0(intermediate_path,"wc_claim_freq.csv"))
wc_claim_sev    <- read_csv(paste0(intermediate_path,"wc_claim_sev.csv"))

# Cut suffix from character columns
wc_claim_freq <- wc_claim_freq %>% mutate(across(where(is.character),~ sub("_.*", "", .x)))
wc_claim_sev <- wc_claim_sev %>% mutate(across(where(is.character), ~ sub("_.*", "", .x)))

# From freq table drop out-of-range values and null policy id rows
wc_claim_freq_right <- wc_claim_freq %>%
  mutate(
    experience_yrs = if_else(experience_yrs >= 0.2 & experience_yrs <= 40, experience_yrs, NA_real_),
    exposure = if_else( exposure >= 0 & exposure <= 1, exposure, NA_real_),
    accident_history_flag = if_else(accident_history_flag %in% 0:1, accident_history_flag, NA_real_),
    psych_stress_index = if_else(psych_stress_index %in% 1:5, psych_stress_index, NA_real_),
    hours_per_week = if_else(hours_per_week %in% c(20, 25, 30, 35, 40), hours_per_week, NA_real_),
    supervision_level = if_else( supervision_level >= 0 & supervision_level <= 1, supervision_level, NA_real_),
    gravity_level = if_else( gravity_level >= 0.75 & gravity_level <= 1.5, gravity_level, NA_real_),
    safety_training_index = if_else(safety_training_index %in% 1:5, safety_training_index, NA_real_),
    protective_gear_quality = if_else(protective_gear_quality %in% 1:5, protective_gear_quality, NA_real_),
    base_salary = if_else( base_salary >= 20000 & base_salary <= 130000, base_salary, NA_real_),
    claim_count = if_else(claim_count %in% 1:3, claim_count, NA_real_)
  )

# Left join freq to sev table
wc_sev_model <- wc_claim_sev %>%
  mutate(
    experience_yrs = if_else(experience_yrs >= 0.2 & experience_yrs <= 40, experience_yrs, NA_real_),
    exposure = if_else(exposure >= 0 & exposure <= 1, exposure, NA_real_),
    accident_history_flag = if_else(accident_history_flag %in% 0:1, accident_history_flag, NA_real_),
    psych_stress_index = if_else(psych_stress_index %in% 1:5, psych_stress_index, NA_real_),
    hours_per_week = if_else(hours_per_week %in% c(20, 25, 30, 35, 40), hours_per_week, NA_real_),
    supervision_level = if_else(supervision_level >= 0 & supervision_level <= 1, supervision_level, NA_real_),
    gravity_level = if_else(gravity_level >= 0.75 & gravity_level <= 1.5, gravity_level, NA_real_),
    safety_training_index = if_else(safety_training_index %in% 1:5, safety_training_index, NA_real_),
    protective_gear_quality = if_else(protective_gear_quality %in% 1:5, protective_gear_quality, NA_real_),
    base_salary = if_else(base_salary >= 20000 & base_salary <= 130000, base_salary, NA_real_),
    claim_length = if_else(claim_length >= 3 & claim_length <= 1000, claim_length, NA_real_), 
    claim_amount = if_else(claim_amount >= 5 , claim_amount, NA_real_)
  ) %>%
  left_join(
    wc_claim_freq_right,
    by = c("policy_id", "worker_id"),
    suffix = c("_sev", "_freq")
  ) %>%
  mutate(
    station_id     = coalesce(station_id_sev, station_id_freq),
    solar_system   = coalesce(solar_system_sev, solar_system_freq),
    occupation     = coalesce(occupation_sev, occupation_freq),
    employment_type= coalesce(employment_type_sev, employment_type_freq),
    
    experience_yrs          = coalesce(experience_yrs_sev, experience_yrs_freq),
    exposure                = coalesce(exposure_sev, exposure_freq),
    accident_history_flag   = coalesce(accident_history_flag_sev, accident_history_flag_freq),
    psych_stress_index      = coalesce(psych_stress_index_sev, psych_stress_index_freq),
    hours_per_week          = coalesce(hours_per_week_sev, hours_per_week_freq),
    supervision_level       = coalesce(supervision_level_sev, supervision_level_freq),
    gravity_level           = coalesce(gravity_level_sev, gravity_level_freq),
    safety_training_index   = coalesce(safety_training_index_sev, safety_training_index_freq),
    protective_gear_quality = coalesce(protective_gear_quality_sev, protective_gear_quality_freq),
    base_salary             = coalesce(base_salary_sev, base_salary_freq),
  ) %>%
  dplyr::select(-ends_with("_freq"), -ends_with("_sev"))

#### EXPORT DATA #####
write.csv(wc_freq_model, paste0(output_path, "wc_freq_model.csv"), row.names = FALSE)
write.csv(wc_sev_model, paste0(output_path, "wc_sev_model.csv"), row.names = FALSE)
