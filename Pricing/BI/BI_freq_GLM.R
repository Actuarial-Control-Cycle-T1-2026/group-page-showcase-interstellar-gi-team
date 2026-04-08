library(copula)

set.seed(123)
n_sim <- 100000

# Inputs:
# agg_BI, agg_WC, agg_EQF, agg_Cargo

q_emp <- function(x, u) {
  quantile(x, probs = u, type = 8, names = FALSE)
}

# =====================================================
# 1) Independent case
# =====================================================
BI_ind    <- sample(agg_BI,    n_sim, replace = TRUE)
WC_ind    <- sample(agg_WC,    n_sim, replace = TRUE)
EQF_ind   <- sample(agg_EQF,   n_sim, replace = TRUE)
Cargo_ind <- sample(agg_Cargo, n_sim, replace = TRUE)

total_indep <- BI_ind + WC_ind + EQF_ind + Cargo_ind

quantile(total_indep, c(0.5, 0.9, 0.95, 0.99, 0.995))

# =====================================================
# 2) Dependent case
# Gumbel for BI, WC, EQF
# Gaussian for (BI+WC+EQF) with Cargo
# =====================================================

# ---------- Step A: Gumbel block for BI, WC, EQF ----------
theta_g <- 2   # example parameter, must be >= 1

gumbel_3d <- gumbelCopula(param = theta_g, dim = 3)
U_g <- rCopula(n_sim, gumbel_3d)

BI_g  <- q_emp(agg_BI,  U_g[, 1])
WC_g  <- q_emp(agg_WC,  U_g[, 2])
EQF_g <- q_emp(agg_EQF, U_g[, 3])

sum_3 <- BI_g + WC_g + EQF_g

# ---------- Step B: Gaussian link between sum_3 and Cargo ----------
# We do this by ranking sum_3, converting to uniforms,
# then generating Cargo conditional on those uniforms.

rho_sum3_cargo <- 0.4   # example Gaussian dependence parameter

# Convert sum_3 to pseudo-uniforms via ranks
U_sum3 <- rank(sum_3, ties.method = "average") / (n_sim + 1)

# Gaussian copula step
Z_sum3 <- qnorm(U_sum3)
eps <- rnorm(n_sim)
Z_cargo <- rho_sum3_cargo * Z_sum3 + sqrt(1 - rho_sum3_cargo^2) * eps
U_cargo <- pnorm(Z_cargo)

Cargo_dep <- q_emp(agg_Cargo, U_cargo)

total_dep <- sum_3 + Cargo_dep

quantile(total_dep, c(0.5, 0.9, 0.95, 0.99, 0.995))