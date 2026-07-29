# File reading
source("sim_FARMA.R")
setwd("~/Desktop/bootstrap bias correction/Point Estimation")
source("local.W.R")
source("sieve_bootstrap.R")
source("LPWN.R")
source("Sieve-bootstrapped corrected LW estimation.R")


# FAR(1)

## Simulation setting
grid <- expand.grid(
  T = c(500, 1000, 2000),
  ma_strength = c(0.2, 0.5, 0.8),
  d = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1, 1.2, 1.4)
)

grid <- expand.grid(
  T = c(500),
  ma_strength = c(0.8),
  d = c(0.1)
)

plan(multisession, workers = 10)
handlers("progress")
LPWN_LW_sim_comparison_FMA <- parallel_sim_LPWN_FMA(n_rep = 20)
saveRDS(LPWN_LW_sim_comparison_FMA, file = "LPWN_LW_DFA_sim_comparison_FMA.rds")



