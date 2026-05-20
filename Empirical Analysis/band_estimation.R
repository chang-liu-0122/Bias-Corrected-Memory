# interval score

# holdout : holdout data vector
# lb      : lower prediction interval vector
# ub      : upper prediction interval vector
# alpha   : significance level (e.g. 0.10 for 90% PI)

interval_score <- function(holdout, lb, ub, alpha) {
  lb_ind <- ifelse(holdout < lb, 1, 0)
  ub_ind <- ifelse(holdout > ub, 1, 0)
  score  <- (ub - lb) +
    2 / alpha * ((lb - holdout) * lb_ind + (holdout - ub) * ub_ind)
  cover <- 1 - (length(which(lb_ind == 1)) + length(which(ub_ind == 1))) /
    length(holdout)
  cpd <- abs(cover - (1 - alpha))
  return(c(cover, cpd, mean(score)))
}



# Helper: extract leading PC scores
get_scores <- function(X) {
  Xc      <- t(scale(t(X), center = TRUE, scale = FALSE))
  cov_mat <- (Xc %*% t(Xc)) / ncol(X)
  lead    <- leading_pc(X, cov_mat)   # assumed available in global env
  lead$scores
}



# Single-level sieve bootstrap distribution for {LW, DFA, LPWN(1), LPWN(2)}
boot_dist_simple <- function(
    X,
    d_current,          # point estimate used for differencing
    estimator = c("LW", "DFA", "R1", "R2"),
    m_const   = 0.65,
    R_noise   = 0,
    B         = 100,
    burn_in   = 200,
    p         = NULL
) {
  estimator <- match.arg(estimator)

  X_diff <- t(FracDiff(t(X), d = d_current))

  # Generate all bootstrap replicates once on the main process
  X_boot_obj <- sieve_bootstrap(
    fun_dat                = t(X_diff),
    ncomp_porder_selection = "CPV_AICC",
    CPV_percent            = 0.95,
    VAR_type               = "none",
    B                      = B,
    burn_in                = burn_in
  )

  # Parallelise the estimation step across B replicates
  d_boot <- future_lapply(seq_len(B), function(b) {

    tryCatch({

      Xb_diff <- X_boot_obj$X_boot[, , b]
      Xb      <- t(FracDiff(t(Xb_diff), d = -d_current))
      scores  <- get_scores(Xb)

      switch(
        estimator,
        LW  = local_whittle_general(scores, m_const),
        DFA = peng_general(scores, m_const = m_const),
        R1  = LPWN_general(scores, m_const, R_short = 1, R_noise),
        R2  = LPWN_general(scores, m_const, R_short = 2, R_noise)
      )

    }, error = function(e) {
      NA_real_
    })

  }, future.seed = FALSE)

  if (!is.null(p)) for (i in seq_len(B)) p()

  unlist(d_boot)
}


# Compute BC-LPWN point estimate
bc_lpwn_point <- function(
    X,
    R_short  = 2,
    m_const  = 0.65,
    R_noise  = 0,
    B_inner  = 100,
    burn_in  = 200
) {
  scores  <- get_scores(X)
  d_hat   <- LPWN_general(scores, m_const, R_short = R_short, R_noise = R_noise)

  est_tag <- ifelse(R_short == 1, "R1", "R2")

  boot_inner <- boot_dist_simple(
    X,
    d_current = d_hat,
    estimator = est_tag,
    m_const   = m_const,
    R_noise   = R_noise,
    B         = B_inner,
    burn_in   = burn_in
  )

  bias_hat <- mean(boot_inner, na.rm = TRUE) - d_hat
  d_bc     <- d_hat - bias_hat

  list(
    d_hat      = d_hat,
    bias_hat   = bias_hat,
    d_bc       = d_bc,
    boot_inner = boot_inner
  )
}



# Nested bootstrap distribution for BC-LPWN

boot_dist_bc_lpwn <- function(
    X,
    R_short   = 2,
    d_bc_hat,
    m_const   = 0.65,
    R_noise   = 0,
    B_outer   = 100,
    B_inner   = 50,
    burn_in   = 200,
    p         = NULL
) {
  # Difference by the BC estimate (best available approximation of true d)
  X_diff <- t(FracDiff(t(X), d = d_bc_hat))

  # Generate all outer replicates once on the main process
  X_boot_outer <- sieve_bootstrap(
    fun_dat                = t(X_diff),
    ncomp_porder_selection = "CPV_AICC",
    CPV_percent            = 0.95,
    VAR_type               = "none",
    B                      = B_outer,
    burn_in                = burn_in
  )

  # Parallelise the outer loop; inner BC stays sequential per worker
  d_bc_outer <- future_lapply(seq_len(B_outer), function(b) {

    tryCatch({

      Xb_diff <- X_boot_outer$X_boot[, , b]
      Xb      <- t(FracDiff(t(Xb_diff), d = -d_bc_hat))

      out_b <- bc_lpwn_point(
        Xb,
        R_short = R_short,
        m_const = m_const,
        R_noise = R_noise,
        B_inner = B_inner,
        burn_in = burn_in
      )

      out_b$d_bc

    }, error = function(e) {
      NA_real_
    })

  }, future.seed = FALSE)

  if (!is.null(p)) for (i in seq_len(B_outer)) p()

  unlist(d_bc_outer)
}


# Main function: estimate d + compute sieve bootstrap bands

estimate_band_all_methods <- function(
    X,
    m_const = 0.65,
    R_noise = 0,
    B       = 100,
    B_outer = 50,
    B_inner = 30,
    burn_in = 200,
    alpha   = 0.10
) {

  progressr::with_progress({

    total_steps <- 4 * B + 2 * B_outer
    p <- progressr::progressor(steps = total_steps)

    # Point estimates
    scores <- get_scores(X)

    d_LW   <- local_whittle_general(scores, m_const)
    d_DFA  <- peng_general(scores, m_const = m_const)
    d_R1   <- LPWN_general(scores, m_const, R_short = 1, R_noise)
    d_R2   <- LPWN_general(scores, m_const, R_short = 2, R_noise)

    # BC-LPWN point estimates
    bc_R1 <- bc_lpwn_point(X, R_short = 1, m_const = m_const,
                           R_noise = R_noise, B_inner = B_inner,
                           burn_in = burn_in)
    bc_R2 <- bc_lpwn_point(X, R_short = 2, m_const = m_const,
                           R_noise = R_noise, B_inner = B_inner,
                           burn_in = burn_in)

    d_R1_bc <- bc_R1$d_bc
    d_R2_bc <- bc_R2$d_bc

    point_estimates <- c(
      d_LW    = d_LW,
      d_DFA   = d_DFA,
      d_R1    = d_R1,
      d_R2    = d_R2,
      d_R1_bc = d_R1_bc,
      d_R2_bc = d_R2_bc
    )

    # bootstrap distributions (LW, DFA, R1, R2)
    boot_LW  <- boot_dist_simple(X, d_LW,  "LW",  m_const, R_noise, B, burn_in, p)
    boot_DFA <- boot_dist_simple(X, d_DFA, "DFA", m_const, R_noise, B, burn_in, p)
    boot_R1  <- boot_dist_simple(X, d_R1,  "R1",  m_const, R_noise, B, burn_in, p)
    boot_R2  <- boot_dist_simple(X, d_R2,  "R2",  m_const, R_noise, B, burn_in, p)

    boot_df <- data.frame(
      boot_id = seq_len(B),
      d_LW    = boot_LW,
      d_DFA   = boot_DFA,
      d_R1    = boot_R1,
      d_R2    = boot_R2
    )

    # Nested bootstrap distributions for BC-LPWN(1) & (2)
    boot_R1_bc_nested <- boot_dist_bc_lpwn(
      X, R_short = 1, d_bc_hat = d_R1_bc,
      m_const = m_const, R_noise = R_noise,
      B_outer = B_outer, B_inner = B_inner, burn_in = burn_in, p = p
    )

    boot_R2_bc_nested <- boot_dist_bc_lpwn(
      X, R_short = 2, d_bc_hat = d_R2_bc,
      m_const = m_const, R_noise = R_noise,
      B_outer = B_outer, B_inner = B_inner, burn_in = burn_in, p = p
    )

    nested_boot_df <- data.frame(
      boot_id = seq_len(B_outer),
      d_R1_bc = boot_R1_bc_nested,
      d_R2_bc = boot_R2_bc_nested
    )

    # Band construction

    make_band <- function(d_boot) {
      lb <- unname(quantile(d_boot, alpha / 2,       na.rm = TRUE))
      ub <- unname(quantile(d_boot, 1 - alpha / 2,   na.rm = TRUE))
      c(lb = lb, ub = ub)
    }

    band_LW    <- make_band(boot_LW)
    band_DFA   <- make_band(boot_DFA)
    band_R1    <- make_band(boot_R1)
    band_R2    <- make_band(boot_R2)
    band_R1_bc <- make_band(boot_R1_bc_nested)
    band_R2_bc <- make_band(boot_R2_bc_nested)

    band_df <- data.frame(
      method  = c("LW", "DFA", "LPWN1", "LPWN2", "BC_LPWN1", "BC_LPWN2"),
      d_hat   = c(d_LW, d_DFA, d_R1, d_R2, d_R1_bc, d_R2_bc),
      lb      = c(band_LW["lb"],    band_DFA["lb"],
                  band_R1["lb"],    band_R2["lb"],
                  band_R1_bc["lb"], band_R2_bc["lb"]),
      ub      = c(band_LW["ub"],    band_DFA["ub"],
                  band_R1["ub"],    band_R2["ub"],
                  band_R1_bc["ub"], band_R2_bc["ub"]),
      width   = c(band_LW["ub"]    - band_LW["lb"],
                  band_DFA["ub"]   - band_DFA["lb"],
                  band_R1["ub"]    - band_R1["lb"],
                  band_R2["ub"]    - band_R2["lb"],
                  band_R1_bc["ub"] - band_R1_bc["lb"],
                  band_R2_bc["ub"] - band_R2_bc["lb"]),
      alpha   = alpha,
      row.names = NULL
    )

    list(
      point       = point_estimates,
      boot        = boot_df,
      nested_boot = nested_boot_df,
      band        = band_df,
      # inner bootstrap draws saved for diagnostics
      inner_boot_R1_bc = bc_R1$boot_inner,
      inner_boot_R2_bc = bc_R2$boot_inner
    )

  })
}
