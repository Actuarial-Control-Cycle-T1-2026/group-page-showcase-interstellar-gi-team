##### This code is to generate freq and sev tables of BI for GLM Modelling ######

# Import Libraries and Tables
library(readr)
library(dplyr)
library(tidyr)

# Set Paths
intermediate_path <- paste0(getwd(), '/00_DataPrep/IntermediateOutput/')
output_path <- paste0(getwd(), '/00_DataPrep/OutputData/')

# Import Intermediate Data
bi_claim_freq    <- read_csv(paste0(intermediate_path,"bi_claim_freq.csv"))
bi_claim_sev    <- read_csv(paste0(intermediate_path,"bi_claim_sev.csv"))

##### FREQUENCY TABLE PIPELINE ####

# Is policy_id unique in left table
n_distinct(bi_claim_freq$policy_id, na.rm = TRUE) == sum(!is.na(bi_claim_freq$policy_id)) #TRUE

# Null Policy_id
any(is.na(bi_claim_freq$policy_id)) # There is na policyid

# Select overlap cols for claim_sev right table
common_cols <- intersect(names(bi_claim_sev), names(bi_claim_freq))
bi_claim_sev_right <- bi_claim_sev %>%
  dplyr::select(common_cols)

# Fill NA for out-of-range values
bi_claim_sev_right <- bi_claim_sev_right %>%
  mutate(
    production_load = if_else(production_load >= 0 & production_load <= 1, production_load, NA_real_),
    exposure = if_else( exposure >= 0 & exposure <= 1, exposure, NA_real_),
    energy_backup_score = if_else(energy_backup_score %in% 1:5, energy_backup_score, NA_real_),
    safety_compliance = if_else(safety_compliance %in% 1:5, safety_compliance, NA_real_)
  )

# Any policy_id with different values?
a <- bi_claim_sev_right %>%
  group_by(policy_id) %>%
  summarise(
    n_station_id          = n_distinct(station_id, na.rm = TRUE),
    n_solar_system        = n_distinct(solar_system, na.rm = TRUE),
    n_production_load     = n_distinct(production_load, na.rm = TRUE),
    n_exposure            = n_distinct(exposure, na.rm = TRUE),
    n_energy_backup_score = n_distinct(energy_backup_score, na.rm = TRUE),
    n_safety_compliance   = n_distinct(safety_compliance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    n_station_id > 1 |
      n_solar_system > 1 |
      n_production_load > 1 |
      n_exposure > 1 |
      n_energy_backup_score > 1 |
      n_safety_compliance > 1
  ) # only one exception (manual edit)

# Group sev_right table by policy_id
bi_claim_sev_policy <- bi_claim_sev_right %>%
  group_by(policy_id) %>%
  summarise(
    claim_count = n(),
    station_id   = first(na.omit(station_id)),
    solar_system = first(na.omit(solar_system)),
    production_load = if (all(is.na(production_load))) NA_real_
    else max(production_load, na.rm = TRUE),
    exposure = if (all(is.na(exposure))) NA_real_
    else first(na.omit(exposure)),
    energy_backup_score = if (all(is.na(energy_backup_score))) NA_real_
    else first(na.omit(energy_backup_score)),
    safety_compliance = if (all(is.na(safety_compliance))) NA_real_
    else first(na.omit(safety_compliance)),
    .groups = "drop"
  )

# Left Join on freq table
bi_freq_model <- bi_claim_freq %>%
  mutate(
    production_load = if_else(between(production_load, 0, 1), production_load, NA_real_),
    avg_crew_exp = if_else(between(avg_crew_exp, 1, 30), avg_crew_exp, NA_real_),
    supply_chain_index = if_else(between(supply_chain_index, 0, 1), supply_chain_index, NA_real_),
    exposure        = if_else(between(exposure, 0, 1), exposure, NA_real_),
    energy_backup_score = if_else(energy_backup_score %in% 1:5, energy_backup_score, NA_real_),
    safety_compliance   = if_else(safety_compliance %in% 1:5, safety_compliance, NA_real_),
    claim_count         = if_else(claim_count %in% 0:4, claim_count, NA_real_),
    maintenance = if_else(maintenance_freq %in% 0:6, maintenance_freq, NA_real_)
  ) %>%
  left_join(
    bi_claim_sev_policy %>%
      dplyr::select(policy_id, station_id, solar_system, 
                    production_load, exposure,
                    energy_backup_score, safety_compliance, claim_count),
    by = "policy_id",
    suffix = c("_freq", "_sev")
  ) %>%
  mutate(
    station_id      = coalesce(station_id_freq, station_id_sev),
    solar_system             = coalesce(solar_system_freq, solar_system_sev),
    production_load      = coalesce(production_load_freq, production_load_sev),
    exposure             = coalesce(exposure_freq, exposure_sev),
    energy_backup_score  = coalesce(energy_backup_score_freq, energy_backup_score_sev),
    safety_compliance    = coalesce(safety_compliance_freq, safety_compliance_sev),
    claim_count          = coalesce(claim_count_freq, claim_count_sev)
  ) %>%
  dplyr::select(-ends_with("_freq"), -ends_with("_sev"))

# Data cleaning
a <- bi_freq_model %>%
  filter(is.na(claim_count)) %>%
  semi_join(bi_claim_sev_policy, by = "policy_id")

bi_freq_model <- bi_freq_model %>%
  mutate(claim_count = coalesce(claim_count, 0))

colSums(is.na(bi_freq_model))

sum(
  rowSums(
    is.na(bi_freq_model[ , setdiff(names(bi_freq_model), "policy_id")])
  ) > 0
)

bi_freq_model <- bi_freq_model %>%
  dplyr::select(-policy_id) %>%
  drop_na()

##### SEVERITY TABLE PIPELINE #####
# Import Tables
bi_claim_freq    <- read_csv(paste0(intermediate_path,"bi_claim_freq.csv"))
bi_claim_sev    <- read_csv(paste0(intermediate_path,"bi_claim_sev.csv"))

#Char cols formatting
bi_claim_freq <- bi_claim_freq %>% mutate(across(where(is.character), ~ sub("_.*", "", .x)))
bi_claim_sev <- bi_claim_sev %>% mutate(across(where(is.character), ~ sub("_.*", "", .x)))

bi_claim_freq_right <- bi_claim_freq %>%
  filter(!is.na(policy_id)) %>%
  mutate(
    production_load = if_else(
      production_load >= 0 & production_load <= 1,
      production_load,
      NA_real_
    ),
    energy_backup_score = if_else(
      energy_backup_score %in% 1:5,
      energy_backup_score,
      NA_real_
    ),
    safety_compliance = if_else(
      safety_compliance %in% 1:5,
      safety_compliance,
      NA_real_
    ),
    supply_chain_index = if_else(
      supply_chain_index >= 0 & supply_chain_index <= 1,
      supply_chain_index,
      NA_real_
    ),
    avg_crew_exp = if_else(
      avg_crew_exp >= 1 & avg_crew_exp <= 30,
      avg_crew_exp,
      NA_real_
    ),
    
  )

bi_sev_model <- bi_claim_sev %>%
  mutate(
    production_load = if_else(between(production_load, 0, 1), production_load, NA_real_),
    energy_backup_score = if_else(energy_backup_score %in% 1:5, energy_backup_score, NA_real_),
    safety_compliance   = if_else(safety_compliance %in% 1:5, safety_compliance, NA_real_)
  ) %>%
  left_join(
    bi_claim_freq_right %>%
      dplyr::select(policy_id,
                    solar_system,
                    station_id,
                    production_load,
                    energy_backup_score,
                    safety_compliance,
                    supply_chain_index,
                    avg_crew_exp,
                    maintenance_freq) %>%
      mutate(
        supply_chain_index = if_else(between(supply_chain_index, 0, 1), supply_chain_index, NA_real_),
        avg_crew_exp = if_else(between(avg_crew_exp, 1, 30), avg_crew_exp, NA_real_),
        maintenance_freq = if_else(between(maintenance_freq, 0, 6), maintenance_freq, NA_real_))
    ,
    by = "policy_id",
    suffix = c("_sev", "_freq")
  ) %>%
  mutate(
    solar_system     = coalesce(solar_system_sev, solar_system_freq),
    station_id     = coalesce(station_id_sev, station_id_freq),
    production_load     = coalesce(production_load_sev, production_load_freq),
    energy_backup_score = coalesce(energy_backup_score_sev, energy_backup_score_freq),
    safety_compliance   = coalesce(safety_compliance_sev, safety_compliance_freq)
  ) %>%
  dplyr::select(
    -station_id_sev, -station_id_freq,
    -solar_system_sev, -solar_system_freq,
    -production_load_sev, -production_load_freq,
    -energy_backup_score_sev, -energy_backup_score_freq,
    -safety_compliance_sev, -safety_compliance_freq
  )

colSums(is.na(bi_sev_model))


bi_sev_model <- bi_sev_model %>%
  dplyr::select(-policy_id,
                -exposure,
                -claim_id,
                -claim_seq) %>%
  drop_na()

max(bi_sev_model$claim_amount)

bi_sev_model <- bi_sev_model %>%
  filter(claim_amount > 0)


#### EXPORT DATA #####
write.csv(bi_freq_model, paste0(output_path, "bi_freq_model.csv"), row.names = FALSE)
write.csv(bi_sev_model, paste0(output_path, "bi_sev_model.csv"), row.names = FALSE)
