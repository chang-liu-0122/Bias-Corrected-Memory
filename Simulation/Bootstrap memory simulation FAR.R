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
  ar_strength = c(0.2, 0.5, 0.8),
  d = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1, 1.2, 1.4)
)

plan(multisession, workers = 100)
handlers("progress")
LPWN_LW_sim_comparison_FAR <- parallel_sim_LPWN_FAR(n_rep = 100)
saveRDS(LPWN_LW_sim_comparison_FAR, file = "LPWN_LW_sim_comparison_FAR.rds")










