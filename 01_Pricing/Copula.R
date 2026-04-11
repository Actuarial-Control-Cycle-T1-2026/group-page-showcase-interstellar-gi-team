library(VineCopula)

set.seed(123)

# =========================================================
# Inputs: aggregate loss vectors
# =========================================================
agg_BI    <- readRDS(file.path(getwd(), "bi_base_agg_loss.rds"))
agg_Cargo <- readRDS(file.path(getwd(), "cargo_base_agg_loss.rds"))
agg_EQF   <- readRDS(file.path(getwd(), "ef_base_agg_loss.rds"))
agg_WC    <- readRDS(file.path(getwd(), "wc_base_agg_loss.rds"))

# If some objects are data frames / lists with aggregate_loss column
if (is.list(agg_BI) || is.data.frame(agg_BI)) {
  if ("aggregate_loss" %in% names(agg_BI)) agg_BI <- agg_BI$aggregate_loss
}
if (is.list(agg_Cargo) || is.data.frame(agg_Cargo)) {
  if ("aggregate_loss" %in% names(agg_Cargo)) agg_Cargo <- agg_Cargo$aggregate_loss
}
if (is.list(agg_EQF) || is.data.frame(agg_EQF)) {
  if ("aggregate_loss" %in% names(agg_EQF)) agg_EQF <- agg_EQF$aggregate_loss
}
if (is.list(agg_WC) || is.data.frame(agg_WC)) {
  if ("aggregate_loss" %in% names(agg_WC)) agg_WC <- agg_WC$aggregate_loss
}

# Force numeric
agg_BI    <- as.numeric(agg_BI)
agg_Cargo <- as.numeric(agg_Cargo)
agg_EQF   <- as.numeric(agg_EQF)
agg_WC    <- as.numeric(agg_WC)

# Scale
agg_BI    <- 1.3*as.numeric(agg_BI)
agg_Cargo <- 1.3*as.numeric(agg_Cargo)
agg_EQF   <- 1.3*as.numeric(agg_EQF)
agg_WC    <- 1.1*as.numeric(agg_WC)

# Remove NA / non-finite just in case
agg_BI    <- agg_BI[is.finite(agg_BI)]
agg_Cargo <- agg_Cargo[is.finite(agg_Cargo)]
agg_EQF   <- agg_EQF[is.finite(agg_EQF)]
agg_WC    <- agg_WC[is.finite(agg_WC)]

n_sim <- 100000

# =========================================================
# Helpers
# =========================================================
q_emp <- function(x, u) {
  quantile(x, probs = u, type = 8, names = FALSE, na.rm = TRUE)
}

tvar <- function(x, p) {
  v <- quantile(x, p, names = FALSE, na.rm = TRUE)
  mean(x[x >= v], na.rm = TRUE)
}

# =========================================================
# Independent benchmark
# =========================================================
total_indep <-
  sample(agg_BI,    n_sim, replace = TRUE) +
  sample(agg_Cargo, n_sim, replace = TRUE) +
  sample(agg_EQF,   n_sim, replace = TRUE) +
  sample(agg_WC,    n_sim, replace = TRUE)

# =========================================================
# BI-centred C-vine
#
# Variable order:
# 1 = BI
# 2 = Cargo
# 3 = EQF
# 4 = WC
#
# In a 4D C-vine, the 6 pair copulas are:
# 1: (1,2)         = BI-Cargo
# 2: (1,3)         = BI-EQF
# 3: (1,4)         = BI-WC
# 4: (2,3 | 1)     = Cargo-EQF | BI
# 5: (2,4 | 1)     = Cargo-WC  | BI
# 6: (3,4 | 1,2)   = EQF-WC | BI,Cargo
#
# family codes:
# 0 = Independence
# 1 = Gaussian
# 4 = Gumbel
# =========================================================
order <- c(1, 2, 3, 4)

family <- c(
  4,  # BI-Cargo
  4,  # BI-EQF
  1,  # BI-WC
  4,  # Cargo-EQF | BI
  1,  # Cargo-WC  | BI
  1   # EQF-WC | BI,Cargo
)

par <- c(
  2,  # BI-Cargo Gumbel
  2,  # BI-EQF   Gumbel
  0.3,  # BI-WC    Gaussian
  1.5,  # Cargo-EQF | BI Gumbel
  0.10,  # Cargo-WC  | BI weak Gaussian
  0.10   # EQF-WC | BI,Cargo weak Gaussian
)

par2 <- rep(0, length(family))

# Build valid C-vine object
RVM <- C2RVine(order = order, family = family, par = par, par2 = par2)

print(summary(RVM))

# =========================================================
# Simulate dependent uniforms from vine copula
# =========================================================
U <- RVineSim(n_sim, RVM)

# =========================================================
# Map uniforms to empirical marginals
# =========================================================
loss_BI    <- q_emp(agg_BI,    U[, 1])
loss_Cargo <- q_emp(agg_Cargo, U[, 2])
loss_EQF   <- q_emp(agg_EQF,   U[, 3])
loss_WC    <- q_emp(agg_WC,    U[, 4])

total_dep <- loss_BI + loss_Cargo + loss_EQF + loss_WC

# =========================================================
# Summaries
# =========================================================
probs <- c(0.50, 0.90, 0.95, 0.99, 0.995)

results <- rbind(
  Independent = quantile(total_indep, probs = probs, names = FALSE, na.rm = TRUE),
  Dependent   = quantile(total_dep,   probs = probs, names = FALSE, na.rm = TRUE)
)

colnames(results) <- paste0(probs * 100, "%")
print(results)

mean(total_dep > 26832255617)
mean(total_dep > 26676877004)

risk_table <- data.frame(
  Case     = c("Independent", "Dependent"),
  Mean     = c(mean(total_indep, na.rm = TRUE), mean(total_dep, na.rm = TRUE)),
  SD       = c(sd(total_indep, na.rm = TRUE),   sd(total_dep, na.rm = TRUE)),
  VaR_99   = c(quantile(total_indep, 0.99,  names = FALSE, na.rm = TRUE),
               quantile(total_dep,   0.99,  names = FALSE, na.rm = TRUE)),
  VaR_995  = c(quantile(total_indep, 0.995, names = FALSE, na.rm = TRUE),
               quantile(total_dep,   0.995, names = FALSE, na.rm = TRUE)),
  TVaR_99  = c(tvar(total_indep, 0.99),  tvar(total_dep, 0.99)),
  TVaR_995 = c(tvar(total_indep, 0.995), tvar(total_dep, 0.995))
)

print(risk_table)

# =========================================================
# Diversification comparison
# =========================================================
standalone_var_995 <- c(
  BI    = quantile(agg_BI,    0.995, names = FALSE, na.rm = TRUE),
  Cargo = quantile(agg_Cargo, 0.995, names = FALSE, na.rm = TRUE),
  EQF   = quantile(agg_EQF,   0.995, names = FALSE, na.rm = TRUE),
  WC    = quantile(agg_WC,    0.995, names = FALSE, na.rm = TRUE)
)

diversification_table <- data.frame(
  Metric = c("Sum of standalone VaR 99.5%", "Portfolio VaR 99.5% Independent", "Portfolio VaR 99.5% Dependent"),
  Value  = c(
    sum(standalone_var_995),
    quantile(total_indep, 0.995, names = FALSE, na.rm = TRUE),
    quantile(total_dep,   0.995, names = FALSE, na.rm = TRUE)
  )
)

print(diversification_table)
