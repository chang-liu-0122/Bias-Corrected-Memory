# File reading
source("sim_FARMA.R")
setwd("~/Desktop/bootstrap bias correction/Point Estimation")
source("local.W.R")
source("sieve_bootstrap.R")
source("LPWN.R")
source("Sieve-bootstrapped corrected LW estimation.R")


# FARMA(1,1)

# Simulation setting
grid <- expand.grid(
  T = c(500, 1000, 2000),
  ar_strength = c(0.2, 0.5, 0.8),
  ma_strength = c(0.2, 0.5, 0.8),
  d = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 1.25)
)

plan(multisession, workers = 10)
handlers("progress")
LPWN_LW_sim_comparison_FARMA <- parallel_sim_LPWN_FARMA(n_rep = 10)
saveRDS(LPWN_LW_sim_comparison_FARMA, file = "LPWN_LW_sim_comparison_FARMA.rds")




