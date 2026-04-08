# Model 1: all factor
m1 <- glm(
  log(claim_amount) ~
    supply_chain_index +
    avg_crew_exp +
    maintenance +
    solar_system +
    production_load +
    energy_backup_score +
    safety_compliance,
  family = gaussian(link = "identity"),
  data = df_sev
)

summary(m1) #AIC: 31686

# Model 2
m2 <- glm(
  log(claim_amount) ~
    solar_system,
  family = gaussian(link = "identity"),
  data = df_sev
)

summary(m2) #AIC: 31673
