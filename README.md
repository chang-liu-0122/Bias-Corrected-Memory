<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://www.mq.edu.au">
    <img src="Macquarie_University_logo.png" alt="Macquarie University" width="240" height="240">
  </a>

<h3 align="center">
Bias correction of long-memory estimator of functional time series via the prefiltered sieve bootstrap
</h3>

<p align="center">
Chang Liu · Han Lin Shang
</p>

</div>

---

# Abstract

This paper investigates a bootstrap bias-correction procedure for estimating the long-memory parameter d in fractionally integrated functional time series. The proposed method applies a sieve bootstrap to prefiltered data using an initial estimate of d The local polynomial Whittle estimator with noise (LPWN) is used as the preliminary estimator to reduce bias caused by strong short-range dependence. Through extensive simulation studies, we examine the bias of classical estimators and demonstrate further improvements achieved by the bootstrap enhancement. The proposed method also provides interval estimation for the memory parameter.

---

# Repository Structure

The repository is organised into three main sections:

1. Point Estimation  
2. Simulation  
3. Empirical Analysis  

---

# Point Estimation

This folder contains the core estimation and bootstrap procedures.

## Main Files

| File | Description |
|---|---|
| `local.W.R` | Implementation of: <br> • Local Whittle estimator (LW) <br> • Detrended Fluctuation Analysis (DFA) |
| `LPWN.R` | Implementation of Local Polynomial Whittle estimators by Frederiksen and Nielsen (2008): <br> • LPWN<sub>1</sub> <br> • LPWN<sub>2</sub> |
| `sieve_bootstrap.R` | Functions for the sieve bootstrap procedure for functional time series by Paparoditis (2018). |
| `Sieve-bootstrapped corrected LW estimation.R` | Main implementation of the bootstrap bias-correction framework using LPWN as the initial estimator. <br> Parallel simulation code included. <br> • BC-LPWN<sub>1</sub> <br> • BC-LPWN<sub>2</sub> |

---

# Simulation

This folder contains the Monte Carlo simulation studies.

## Main Files

| File | Description |
|---|---|
| `sim_FARMA.R` | Simulation framework for FARFIMA processes. |
| `Bootstrap memory simulation FAR.R` | Simulation study for FARFIMA(1,d,0) |
| `Bootstrap memory simulation FMA.R` | Simulation study for FARFIMA(0,d,1) |
| `Bootstrap memory simulation FARMA.R` | Simulation study for FARFIMA(1,d,1) |
| `Analysis bootstrap bias correction sim.Rmd` | Analysis and plotting of simulation results |
| `bootstrap bias correction shiny.R` | Shiny app for interactive visualisation of simulation results |

---

## Simulation Result Files

| File | Description |
|---|---|
| `LPWN_LW_sim_comparison_FAR.rds` | Simulation results for FARFIMA(1,d,0) with LW, LPWN<sub>1</sub>, LPWN<sub>2</sub>, BC-LPWN<sub>1</sub>, BC-LPWN<sub>2</sub> |
| `LPWN_LW_sim_comparison_FARMA.rds` | Simulation results for FARFIMA(1,d,1) with LW, LPWN<sub>1</sub>, LPWN<sub>2</sub>, BC-LPWN<sub>1</sub>, BC-LPWN<sub>2</sub> |
| `bootstrap_comparison_peng_FAR.rds` | DFA simulation results for the FARFIMA(1,d,0) setting with LW, DFA |
| `bootstrap_comparison_peng_FARMA.rds` | DFA simulation results for the FARFIMA(1,d,1) setting with LW, DFA |

---

## Plot Folders

| Folder | Description |
|---|---|
| `FAR_Result_Plots` | Figures for FARFIMA(1,d,0) simulations |
| `FMA_Result_Plots` | Figures for FARFIMA(0,d,1) simulations |
| `FARMA_Result_Plots` | Figures for FARFIMA(1,d,1) simulations |

---

# Empirical Analysis

This folder contains empirical applications and interval estimation.

## Main Files

| File | Description |
|---|---|
| `band_estimation.R` | Bootstrap interval estimation procedures |
| `save_function.R` | Helper functions for saving figures |
| `Empirical analysis Bootstrap bias correction.Rmd` | Empirical analysis, plotting, and result summaries |

---

## Empirical Datasets

| File | Description |
|---|---|
| `Mx_1x1_sweden.txt` | Swedish age-specific mortality data |
| `yield_curves full.csv` | Canadian yield curve data |

---

## Empirical Result Files

| File | Description |
|---|---|
| `mortality_res_female.rds` | Bootstrap results for female mortality data |
| `mortality_res_male.rds` | Bootstrap results for male mortality data |
| `yield_result_bias_correction.rds` | Bootstrap results for yield-curve data |

---

## Plot Folder

| Folder | Description |
|---|---|
| `Empirical Plots` | Figures and exploratory plots used in the paper |

---

# Suggested Order of Running Files

## Point Estimation

1. `local.W.R`
2. `LPWN.R`
3. `sieve_bootstrap.R`
4. `Sieve-bootstrapped corrected LW estimation.R`

---

## Simulation

1. `sim_FARMA.R`
2. `Bootstrap memory simulation FAR.R`
3. `Bootstrap memory simulation FMA.R`
4. `Bootstrap memory simulation FARMA.R`
5. `Analysis bootstrap bias correction sim.Rmd`
6. `bootstrap bias correction shiny.R`

---

## Empirical Analysis

1. `band_estimation.R`
2. `save_function.R`
3. `Empirical analysis Bootstrap bias correction.Rmd`

---

# Contact

### Chang Liu

Department of Actuarial Studies and Business Analytics  

Macquarie University  

Email: chang.liu45@students.mq.edu.au

---

### Han Lin Shang

Department of Actuarial Studies and Business Analytics  

Macquarie University  

Email: hanlin.shang@mq.edu.au  



