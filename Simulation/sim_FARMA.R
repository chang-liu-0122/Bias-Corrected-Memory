# standard Brownian Motion [0,1]

BrownMat <- function(N, refinement)
{
    mat <- matrix(nrow = refinement, ncol = N)
    c <- 1
    while(c <= N)
    {
        vec <- BM(N = refinement-1)
        mat[,c] <- vec
        c <- c+1
    }
    return(mat)
}

# functional kernel function in the FAR model

funKernel <- function(ref)
{
  Mat <- matrix(0, ref, ref)

  for(i in 1:ref)
  {
    for(j in 1:ref)
    {
      Mat[i,j] <- exp(0.5 * ((i/ref)^2 + (j/ref)^2))
    }
  }

  # effective operator includes integration weight
  A <- Mat / ref

  # operator norm (largest singular value)
  op_norm <- norm(A, type = "2")

  # scale Mat so that the effective operator has norm 1
  Mat <- Mat / op_norm

  return(Mat)
}


funIntegral = function(ref, Mat, X)
{
    Mat = Mat %*% X
    return(Mat/ref)
}

generate_AR_kernels <- function(ref, p, ar_strength = 0.6)
{
  Phi_list <- list()

  for(i in 1:p)
  {
    Phi_list[[i]] <- funKernel(ref) * (ar_strength^i)
  }

  return(Phi_list)
}

funARMat_p <- function(refinement, eta_star_val, Phi_list)
{
  p <- length(Phi_list)
  Tn <- ncol(eta_star_val)

  res <- matrix(0, refinement, Tn)

  # initialise first p values
  res[,1:p] <- eta_star_val[,1:p]

  for(t in (p+1):Tn)
  {
    ar_sum <- rep(0, refinement)

    for(i in 1:p)
    {
      ar_sum <- ar_sum + funIntegral(refinement, Phi_list[[i]], res[,(t-i)])
    }

    res[,t] <- ar_sum + eta_star_val[,t]
  }

  return(res)
}

MAphi <- function(ref)
{
  Mat <- matrix(0, ref, ref)

  for(i in 1:ref)
  {
    for(j in 1:ref)
    {
      Mat[i,j] <- min(i,j) / ref
    }
  }

  # integral operator
  A <- Mat / ref

  # operator norm
  norm <- norm(A, type = "2")

  Mat <- Mat / norm

  return(Mat)
}

generate_MA_kernels <- function(ref, q, ma_strength = 0.6)
{
  Theta_list <- list()

  base_kernel <- MAphi(ref)

  for(j in 1:q)
  {
    Theta_list[[j]] <- base_kernel * (ma_strength^j)
  }

  return(Theta_list)
}



funMAMat_q <- function(refinement, eta_star_val, Theta_list)
{
  q <- length(Theta_list)
  Tn <- ncol(eta_star_val)

  res <- matrix(0, refinement, Tn)

  for(t in (q+1):Tn)
  {
    ma_sum <- eta_star_val[,t]

    for(j in 1:q)
    {
      ma_sum <- ma_sum +
        funIntegral(refinement, Theta_list[[j]], eta_star_val[,(t-j)])
    }

    res[,t] <- ma_sum
  }

  return(res)
}


### Simulation Through FracDiff

sim_FARMA <- function(
    n = 500,
    d = 0.1,
    no_grid = 101,
    seed_number,
    p,
    q,
    ar_strength = 0.6,
    ma_strength = 0.6
)
{
  set.seed(seed_number)

  # innovations
  eps <- BrownMat(N = n + 100, refinement = no_grid)

  # fractional innovation
  eta_star <- FracDiff(
    x = t(eps),
    d = -d
  ) %>% t()

  # FAR / FARMA
  if(p != 0 & q == 0)
  {
    Phi_list <- generate_AR_kernels(no_grid, p, ar_strength)
    sim_X <- funARMat_p(no_grid, eta_star, Phi_list)

  } else if(p == 0 & q != 0)
  {
    Theta_list <- generate_MA_kernels(no_grid, q, ma_strength)
    sim_X <- funMAMat_q(no_grid, eta_star, Theta_list)

  } else if(p != 0 & q != 0)
  {
    Theta_list <- generate_MA_kernels(no_grid, q, ma_strength)
    Phi_list   <- generate_AR_kernels(no_grid, p, ar_strength)

    eta_star_MA <- funMAMat_q(no_grid, eta_star, Theta_list)
    sim_X <- funARMat_p(no_grid, eta_star_MA, Phi_list)
  }
  else
  {
    stop("process must be FAR / FMA / FARMA")
  }

  # ======================
  # Step 4: output
  # ======================
  sim_X_record = sim_X[,(ncol(sim_X)-(n-1)):ncol(sim_X)]
  eta_record   = eta_star[,(ncol(eta_star)-(n-1)):ncol(eta_star)]

  return(list(
    X   = sim_X_record,
    eta = eta_record
  ))
}



### Simulation Through Beta Coefficient


# sim_FARMA <- function(
#     n = 500,
#     d = 0.1,
#     no_grid = 101,
#     seed_number,
#     p,
#     q,
#     ar_strength = 0.6,
#     ma_strength = 0.6
# )
# {
#   set.seed(seed_number)
#
#   # simulate stochastic process realizations
#
#   step_1 = BrownMat(N = n + 100, refinement = no_grid)
#
#   # calculating beta values
#   beta_val = vector("numeric", n + 100)
#
#
#   if (abs(d) < 1e-8) {
#
#     beta_val[1] <- 1
#     beta_val[2:(n+100)] <- 0
#
#   } else {
#
#     for(i in 1:(n + 100)) {
#       beta_val[i] <- exp(lgamma(i + d) - lgamma(i + 1) - lgamma(d))
#     }
#   }
#
#   # for(i in 1:(n + 100))
#   # {
#   #   beta_val[i] = exp(lgamma(i + d) - lgamma(i + 1) - lgamma(d))
#   # }
#   # rm(i)
#
#   # Step 2
#
#   eta_star = matrix(NA, no_grid, n + 100)
#   for(t_val in 1:(n + 100))
#   {
#     inner_sum = matrix(NA, no_grid, t_val)
#     index = t_val:1
#     for(ik in 1:t_val)
#     {
#       inner_sum[,ik] = (beta_val[ik] * step_1[,index[ik]])
#     }
#     eta_star[,t_val] = rowSums(inner_sum)
#     rm(index); rm(inner_sum)
#   }
#
#   # Step 3
#
#   if(p != 0 & q == 0)
#   {
#     Phi_list <- generate_AR_kernels(no_grid, p, ar_strength)
#     sim_X <- funARMat_p(no_grid, eta_star, Phi_list)
#   }
#
#   else if(p == 0 & q != 0)
#   {
#     Theta_list <- generate_MA_kernels(no_grid, q, ma_strength)
#     sim_X <- funMAMat_q(no_grid, eta_star, Theta_list)
#   }
#
#   else if(p != 0 & q != 0)
#   {
#     Theta_list <- generate_MA_kernels(no_grid, q, ma_strength)
#     Phi_list <- generate_AR_kernels(no_grid, p, ar_strength)
#
#     eta_star_MA <- funMAMat_q(no_grid, eta_star, Theta_list)
#     sim_X <- funARMat_p(no_grid, eta_star_MA, Phi_list)
#   }
#   else
#   {
#     warning("process can either be FAR FMA or FARMA process.")
#   }
#
#   # Step 4 (last n observations)
#
#   sim_X_record = sim_X[,(ncol(sim_X)-(n-1)):ncol(sim_X)]
#   return(list(
#     X = sim_X_record,
#     eta = eta_star[,(ncol(eta_star)-(n-1)):ncol(eta_star)]
#   ))
# }

