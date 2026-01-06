# Purpose: Data generation for simulation.
# Updated: 2025-5-12

#' Data generation.
#'
#' @param n_labeled Number of labeled examples.
#' @param N_unlabeled Number of unlabeled examples.
#' @param prot_att_prevalence Prevalence of the protected attribute
#' If the number of class is more than two, the length of this vector
#' should be equal to the number of class. If the number of class is two,
#' the length of this vector can either be one or two.
#' @param model Indicator to generate from which model.
#' possible options: "scenario 1", "scenario 2"
#' @param rho Correlation between covariates. Default is 0.4.
#' @return Data.frame.
#' @export

DataGeneration <- function(n_labeled,
                           N_unlabeled,
                           prot_att_prevalence,
                           model,
                           rho = 0.4) {
  # Total sample size.
  N_total <- n_labeled + N_unlabeled

  # Dimension
  p <- 16

  # Generate Covariates
  id_matrix <- diag(p)
  ar_one_matrix <- rho^abs(row(id_matrix) - col(id_matrix))
  sigma <- 3 * ar_one_matrix
  mu <- rep(0, p)
  covariates <- MASS::mvrnorm(N_total, mu, Sigma = sigma)
  colnames(covariates) <- c(paste0("X_", 1:10), paste0("W_", 1:5), "A")

  # Placeholder for Y
  Y <- rep(NA, N_total)
  A <- ifelse(covariates[, "A"] > qnorm(1 - prot_att_prevalence), 1, 0)
  W <- covariates[, 11:15]
  X <- covariates[, 1:10]

  # Placeholder for Y
  Y <- rep(NA, N_total)

  # Generate Y
  if (model == "scenario 1") {
    b0 <- matrix(
      c(
        -4, 1, 1, 0.5, 0.5, rep(0, 6), 0.4, 0.4, 0.4, 0, 0,
        -4, 0.9, 0.9, 0.4, 0.4, rep(0, 6), 0.3, 0.3, 0.3, 0, 0
      ),
      nrow = 2, byrow = TRUE
    )
    lin_pred <- cbind(1, X, W) %*% t(b0)
    S <- plogis(
      lin_pred + 0.3 * (X[, 2])^2 - 0.4 * (X[, 3])^3 + 0.1 * X[, 5] * X[, 6]
    )
    for (a in c(0, 1)) {
      Y[A == a] <- rbinom(sum(A == a), 1, S[A == a, (a + 1)])
    }
  }
  if (model == "scenario 2") {
    b0 <- matrix(
      c(
        1.3, 0.4, -0.3, 0.15, -0.15, rep(0, 6), 0.25, -0.2, 0.2, 0, 0,
        1.3, 0.35, -0.25, 0.2, -0.2, rep(0, 6), 0.15, -0.15, 0.2, 0, 0
      ),
      nrow = 2, byrow = TRUE
    )
    lin_pred <- cbind(1, X, W) %*% t(b0)
    for (a in c(0, 1)) {
      S <- exp(-lin_pred[, a + 1]^2)
      Y[A == a] <- rbinom(sum(A == a), 1, S[A == a])
    }
  } else if (model == "scenario 3") {
    ## ---- Build z from X1:5 and W1:3 (plus interactions / higher order) ----
    # Uses X = covariates[,1:10] and W = covariates[,11:15]
    X <- covariates[, 1:10, drop = FALSE]
    W <- covariates[, 11:15, drop = FALSE]

    t <- -0.5 +
      1.00 * X[, 1] + 0.80 * X[, 2] + 0.60 * X[, 3] + 0.50 * X[, 4] + 0.30 * X[, 5] +
      0.30 * W[, 1] + 0.20 * W[, 2] - 0.20 * W[, 3] +
      0.10 * X[, 1] * W[, 1] - 0.10 * X[, 5] * X[, 2] + # interactions
      0.12 * (X[, 3]^2) - 0.08 * (X[, 3]^3) / 3 + 0.10 * (W[, 1]^2) # higher-order
    z <- as.numeric(scale(t)) # center/scale around operating region

    ## ---- Coefficients + “gap” knob (keeps same shape; A=1 slightly larger) ----
    s0 <- 0.18
    gap <- 0.8 # increase to 0.15 for more separation

    b_z <- c(3.20, 3.20 * (1 + gap))
    amp_up_1 <- c(7.50, -12.50 * (1 + gap))
    amp_down <- c(-12.50, 7.5 * (1 + gap))
    amp_up_2 <- c(9.00, 9.00 * (1 + gap))
    ripple_amp <- c(0.60, 0.60 * (1 + gap))
    ripple_k <- 4

    ## ---- Force opposite-signed residual bias by group (phase/warp/shift) ----
    phi <- c(0, pi) # ripple phase: 0 vs 180°
    center_shift <- c(-0.1, 0.3)
    width_mult <- c(1.00, 1.08)
    warp_kappa <- c(-0.1, 0.1)

    ## ---- Nonlinear index g(z, A) ----
    g <- numeric(N_total)
    for (a in 0:1) {
      idx <- which(A == a)
      zz <- z[idx]

      zt <- zz + warp_kappa[a + 1] * (zz^3) + center_shift[a + 1]
      s0a <- s0 * width_mult[a + 1]

      ga <- b_z[a + 1] * zt +
        amp_up_1[a + 1] * exp(-(zt + 0.30)^2 / (2 * s0a^2)) +
        amp_down[a + 1] * exp(-(zt + 0.00)^2 / (2 * s0a^2)) +
        amp_up_2[a + 1] * exp(-(zt - 0.30)^2 / (2 * s0a^2)) +
        ripple_amp[a + 1] * exp(-(zt / 0.50)^2) * sin(2 * pi * ripple_k * plogis(zt) + phi[a + 1])

      g[idx] <- ga
    }

    ## ---- Same inverse link in both groups (cloglog), target prevalence ~30% ----
    p_true <- numeric(N_total)
    for (a in 0:1) {
      idx <- which(A == a)
      ga <- g[idx]
      f <- function(d) mean(1 - exp(-exp(ga + d))) - 0.30
      delta <- tryCatch(
        uniroot(f, interval = c(-20, 20))$root,
        error = function(e) uniroot(f, interval = c(-40, 40))$root
      )
      if (a == 0) {
        p_true[idx] <- 1 - exp(-exp(ga + delta))
      } else {
        p_true[idx] <- pnorm(ga + delta)
      }
    }
    p_true <- pmin(pmax(p_true, 1e-6), 1 - 1e-6)

    ## ---- Sample labels ----
    Y <- rbinom(N_total, 1, p_true)
  } else if (model == "scenario 4") {
    X <- covariates[, 1:10, drop = FALSE]
    W <- covariates[, 11:15, drop = FALSE]
    N_total <- nrow(covariates)
    
    ## -------- Deterministic signal (GLM-learnable) --------
    eta <- (1.00*X[,1] + 0.80*X[,2] + 0.60*X[,3] + 0.50*X[,4] + 0.30*X[,5] +
              0.10*X[,1]*W[,1] - 0.10*X[,5]*X[,2] + 0.30*X[,2]*W[,2] +
              W[,1] + 0.2*W[,2]- 0.20*W[,3] + (W[,1]^2) + (W[,3]^2))
    
    ## -------- GLM-index proxy & score for gating (aligns with Ŝ) --------
    t_glm <- (-0.5 +
                1.00*X[,1] + 0.80*X[,2] + 0.60*X[,3] + 0.50*X[,4] +
                0.30*X[,5] + 0.20*X[,6] + 0.10*X[,7] - 0.10*X[,8] +
                0.05*X[,9]  - 0.05*X[,10])
    z_gate <- as.numeric(scale(t_glm))
    s_gate <- plogis(1.55 * z_gate)             # put plenty of mass around/above 0.5
    
    ## -------- Mild heteroskedastic EV scale (can be swapped for your "weird" sigma) --------
    sigma0 <- 0.55
    sigma  <- sigma0 + 0.35 * exp(-((s_gate - 0.58)/0.055)^2)  # modest spike above 0.5
    sigma  <- pmax(0.12, sigma)
    
    ## -------- EV components (cloglog ↑, reverse-cloglog ↓) --------
    p_inc <- function(x, sig) 1 - exp(-exp(x / sig))
    p_dec <- function(x, sig)     exp(-exp(-x / sig))
    logit <- function(u) log(u) - log1p(-u)
    
    ## -------- Windows in s_gate: center (↑), right (↓), left (↑) -> locally non-monotone just >0.5 --------
    wL <- exp(-((s_gate - 0.42)/0.045)^2)  # left shoulder (increase)
    wC <- exp(-((s_gate - 0.50)/0.030)^2)  # center window (increase)
    wR <- exp(-((s_gate - 0.58)/0.050)^2)  # right shoulder (decrease)
    wsum <- wL + wC + wR + 1e-12
    wL <- wL/wsum; wC <- wC/wsum; wR <- wR/wsum
    
    ## -------- Gentle logit-space notch just above 0.5 (DO NOT crater TPR) --------
    notch_depth <- 0.3   # reduce if TPR dips too much; increase to bias more
    notch <- notch_depth * exp(-((s_gate - 0.57)/0.045)^2)
    
    ## -------- Per-group prevalence ≈ prev (use your 'prev' outside to set target_prev) --------
    target_prev <- prev   # set to 'prev' you pass to DataGeneration
    p_true <- numeric(N_total)
    
    for (a in 0:1) {
      idx <- which(A == a)
      rg  <- eta[idx]               # group-specific shift solved via delta_a
      sg  <- sigma[idx]
      wLg <- wL[idx]; wCg <- wC[idx]; wRg <- wR[idx]
      s   <- s_gate[idx]
      
      f <- function(d) {
        r <- rg + d
        # EV mixture: mostly increasing, but decreasing just above 0.5
        p_mix <- wLg * p_inc(r, sg) +
          wCg * p_inc(r, sg) +
          wRg * p_dec(r, sg)
        
        # logit-space local notch above 0.5 + tiny ripple (kept mild)
        z <- logit(pmax(pmin(p_mix, 1 - 1e-9), 1e-9))
        z <- z - notch + 0.15 * exp(-((s - 0.55)/0.10)^2) * sin(2*pi*5*s)
        
        mean(plogis(z)) - target_prev
      }
      delta_a <- tryCatch(uniroot(f, c(-60, 60))$root,
                          error = function(e) uniroot(f, c(-100, 100))$root)
      
      # Final probabilities for this group
      r  <- rg + delta_a
      p_mix <- wLg * p_inc(r, sg) + wCg * p_inc(r, sg) + wRg * p_dec(r, sg)
      z <- logit(pmax(pmin(p_mix, 1 - 1e-9), 1e-9))
      z <- z - notch + 0.15 * exp(-((s - 0.55)/0.10)^2) * sin(2*pi*5*s) + 1
      p_true[idx] <- pmin(pmax(plogis(z), 1e-6), 1 - 1e-6)
    }
    
    ## -------- Sample labels from the complicated link --------
    Y <- rbinom(N_total, 1, p_true)
  } else if (model == "tree") {
    X <- covariates[, 1:10,  drop = FALSE]
    W <- covariates[, 11:15, drop = FALSE]
    N_total <- nrow(covariates)
    
    # scores for simple rules
    eta  <- X[,1] + X[,2] - X[,3] + W[,1] - W[,2] + W[,3] + X[,4]^2 - X[,5]^2 +
      0.3 * X[,1]*W[,1] - 0.3 * X[,2]*W[,2] + 0.2 * X[,3]*W[,3] +
      tanh(0.4 * W[,5]) - tanh(0.2 * X[,4]) + 0.1 * sin(2 * W[,4] * X[,5])
    eta2 <- W[,1] - W[,2] + W[,3] + 0.4 * W[,4]^2 - 0.3 * W[,5]^2 +
      0.2 * X[,1]*W[,1] - 0.2 * X[,2]*W[,2] + 0.1 * X[,3]*W[,3] +
      tanh(0.5 * X[,5]) - tanh(0.3 * W[,4]) + 0.1 * cos(2 * X[,4] * W[,5])
    
    # --- KEY FIX: group-wise cutpoints so each group has >= one ≥0.5 bucket ---
    t0  <- stats::quantile(eta[A==0],  0.55, names = FALSE)
    u0  <- stats::quantile(eta2[A==0], 0.50, names = FALSE)
    t1  <- stats::quantile(eta[A==1],  0.55, names = FALSE)
    w2c <- stats::quantile(W[A==1, 2], 0.50, names = FALSE)
    
    # tree-like score S on a small grid (unchanged values)
    S <- dplyr::case_when(
      # A=0 → {0.60, 0.40, 0.20, 0.10}
      A == 0 & eta > t0  & eta2 >  u0  ~ 0.80,
      A == 0 & eta > t0  & eta2 <= u0  ~ 0.40,
      A == 0 & eta <= t0 & eta2 >  u0  ~ 0.20,
      A == 0 & eta <= t0 & eta2 <= u0  ~ 0.10,
      
      # A=1 → {0.65, 0.45, 0.25, 0.15}
      A == 1 & eta >  t1 & eta2 >  w2c ~ 0.85,
      A == 1 & eta >  t1 & eta2 <= w2c ~ 0.45,
      A == 1 & eta <= t1 & eta2 >  w2c ~ 0.25,
      A == 1 & eta <= t1 & eta2 <= w2c ~ 0.15,
      
      TRUE ~ 0.05
    )
    
    Y <- rbinom(N_total, 1, S)
  } else if (model == "scenario 5") {
    ## ---- Build z from X1:5 and W1:3 (plus interactions / higher order) ----
    # Uses X = covariates[,1:10] and W = covariates[,11:15]
    X <- covariates[, 1:10, drop = FALSE]
    W <- covariates[, 11:15, drop = FALSE]
    
    t <- -0.5 +
      1.00 * X[, 1] + 0.80 * X[, 2] + 0.60 * X[, 3] + 0.50 * X[, 4] + 0.30 * X[, 5] +
      0.30 * W[, 1] + 0.20 * W[, 2] - 0.20 * W[, 3] +
      0.10 * X[, 1] * W[, 1] - 0.10 * X[, 5] * X[, 2] + # interactions
      0.12 * (X[, 3]^2) - 0.08 * (X[, 3]^3) / 3 + 0.10 * (W[, 1]^2) # higher-order
    z <- as.numeric(scale(t)) # center/scale around operating region
    
    ## ---- Coefficients + “gap” knob (keeps same shape; A=1 slightly larger) ----
    # s0 <- 0.18
    # gap <- 0.8 # increase to 0.15 for more separation
    # 
    # b_z <- c(3.20, 3.20 * (1 + gap))
    # amp_up_1 <- c(7.50, -12.50 * (1 + gap))
    # amp_down <- c(-12.50, 7.5 * (1 + gap))
    # amp_up_2 <- c(9.00, 9.00 * (1 + gap))
    # ripple_amp <- c(0.60, 0.60 * (1 + gap))
    # ripple_k <- 4
    # 
    # ## ---- Force opposite-signed residual bias by group (phase/warp/shift) ----
    # phi <- c(0, pi) # ripple phase: 0 vs 180°
    # center_shift <- c(-0.1, 0.3)
    # width_mult <- c(1.00, 1.08)
    # warp_kappa <- c(-0.1, 0.1)
    # 
    # ## ---- Nonlinear index g(z, A) ----
    # g <- numeric(N_total)
    # for (a in 0:1) {
    #   idx <- which(A == a)
    #   zz <- z[idx]
    #   
    #   zt <- zz + warp_kappa[a + 1] * (zz^3) + center_shift[a + 1]
    #   s0a <- s0 * width_mult[a + 1]
    #   
    #   ga <- b_z[a + 1] * zt + amp_up_1[a + 1] * exp(-(zt - 0.30)^2 / (2 * s0a^2))
    #   #amp_up_1[a + 1] * exp(-(zt + 0.30)^2 / (2 * s0a^2)) +
    #   #amp_down[a + 1] * exp(-(zt + 0.00)^2 / (2 * s0a^2)) +
    #   #amp_up_2[a + 1] * exp(-(zt - 0.30)^2 / (2 * s0a^2)) +
    #   #ripple_amp[a + 1] * exp(-(zt / 0.50)^2) * sin(2 * pi * ripple_k * plogis(zt) + phi[a + 1])
    #   
    #   g[idx] <- ga
    # }
    
    ## ---- Same inverse link in both groups (cloglog), target prevalence ~30% ----
    ## ---- Coefficients + “gap” knob (A=1 is scaled-up mirror of A=0) ----
    s0  <- 0.18
    gap <- 0.8  # increase for more separation; A=1 gets (1+gap) scaling
    
    ## Base (single) coefficients: define the *shape* once
    b_base        <- 3.20
    amp_up_1_base <- 7.50
    # If you later want multiple bumps/ripples, add them inside f() (kept off by default)
    amp_down_base <- -7.50
    amp_up_2_base <- 9.00
    ripple_amp_base <- 0.60
    ripple_k <- 4
    
    ## ---- Symmetry-preserving transform (shared for both groups) ----
    delta  <- 0.10   # center shift (common)
    kappa  <- 0.10   # cubic warp (common)
    width_mult <- 1.00
    s0a <- s0 * width_mult
    
    ## ---- Define the base shape f(zt) once; group 1 will be -f(-zt) * (1+gap) ----
    f <- function(zt) {
      b_base * zt +
        amp_up_1_base * exp(-(zt - 0.30)^2 / (2 * s0a^2))
      # + amp_down_base * exp(-(zt - 0.00)^2 / (2 * s0a^2)) +
      #   amp_up_2_base * exp(-(zt - 0.30)^2 / (2 * s0a^2)) +
      #   ripple_amp_base * exp(-(zt / 0.50)^2) * sin(2 * pi * ripple_k * plogis(zt))
    }
    f2 <- function(zt) {
      b_base * zt +
        amp_up_1_base * exp(-(zt + 0.30)^2 / (2 * s0a^2))
      # + amp_down_base * exp(-(zt - 0.00)^2 / (2 * s0a^2)) +
      #   amp_up_2_base * exp(-(zt - 0.30)^2 / (2 * s0a^2)) +
      #   ripple_amp_base * exp(-(zt / 0.50)^2) * sin(2 * pi * ripple_k * plogis(zt))
    }
    
    ## ---- Nonlinear index g(z, A) with enforced anti-symmetry & scaling ----
    g <- numeric(N_total)
    zt_all <- z + kappa * (z^3) + delta  # same transform for everyone
    zt_all <- z
    
    idx0 <- which(A == 0)
    idx1 <- which(A == 1)
    
    g[idx0] <- f(zt_all[idx0])                    # A = 0
    g[idx1] <- f2(zt_all[idx1])      # A = 1 is mirrored & scaled
    
    p_true <- numeric(N_total)
    for (a in 0:1) {
      idx <- which(A == a)
      ga <- g[idx]
      f <- function(d) mean(1 - exp(-exp(ga + d))) - 0.30
      delta <- tryCatch(
        uniroot(f, interval = c(-20, 20))$root,
        error = function(e) uniroot(f, interval = c(-40, 40))$root
      )
      if (a == 0) {
        p_true[idx] <- 1 - exp(-exp(ga + delta))
      } else {
        p_true[idx] <- 1 - exp(-exp(ga + delta))
      }
    }
    p_true <- pmin(pmax(p_true, 1e-6), 1 - 1e-6)
    
    ## ---- Sample labels ----
    Y <- rbinom(N_total, 1, p_true)
  } 
  # Induce missingness.
  Y_miss <- Y
  Y_miss[sample(c(1:N_total), N_unlabeled, replace = F)] <- NA

  # Simulated data.
  my_data <- cbind(Y = Y, A = A, Y_miss = Y_miss, X = X, W = W)
  my_data <- data.frame(my_data)

  return(my_data)
}
