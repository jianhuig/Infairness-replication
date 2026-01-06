start_time <- Sys.time()

library(dplyr)
library(glmnet)
library(SSFairnessAudit)
library(parallel)

## Honor SLURM's allocation
n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
if (is.na(n_cores) || n_cores < 1L) n_cores <- 1L

source("DataGeneration.R")

# ---------- Parameters
prev      <- 0.4
nclass    <- 2
n         <- 200 * nclass      # labeled
N         <- 1e4 * nclass      # unlabeled
rho       <- 0.4
threshold <- 0.5
model     <- "scenario 5"
nsim      <- 1e3

# ---------- Train S on independent data (shared across workers)
set.seed(1234)
indep <- DataGeneration(
  n_labeled = 1e4,
  N_unlabeled = 0,
  prot_att_prevalence = prev,
  model = model,
  rho = rho
)

model_0 <- glm(
  Y ~ ., family = binomial(),
  data = indep %>% filter(A == 0) %>% select(Y, contains("X"))
)
print(summary(model_0))

model_1 <- glm(
  Y ~ ., family = binomial(),
  data = indep %>% filter(A == 1) %>% select(Y, contains("X"))
)
print(summary(model_1))

# Oracle
# generate the main dataset
dat <- DataGeneration(
  n_labeled = 1e6,
  N_unlabeled = 0,
  prot_att_prevalence = prev,
  model = model,
  rho = rho
)
# using independent models to get S
dat$S <- NA_real_
dat$S[dat$A == 0] <- predict(model_0, newdata = dat %>% filter(A == 0),
                             type = "response")
dat$S[dat$A == 1] <- predict(model_1, newdata = dat %>% filter(A == 1),
                             type = "response")
# prepare main data
dat$C <- ifelse(dat$S > threshold, 1, 0)
labeled   <- dat %>% filter(!is.na(Y_miss))

# supervised on labeled only
oracle <- Audit_Fairness(
  Y = dat$Y, S = dat$S, A = dat$A,
  threshold = threshold,
  method = "supervised"
)
saveRDS(oracle, file = paste0(model, "_oracle",".rds"))

# ---------- One simulation replicate
sim_one <- function(i,
                    n, N, prev, model, rho, threshold,
                    model_0, model_1) {
  
  # generate the main dataset
  dat <- DataGeneration(
    n_labeled = n,
    N_unlabeled = N,
    prot_att_prevalence = prev,
    model = model,
    rho = rho
  )
  
  # using independent models to get S
  dat$S <- NA_real_
  dat$S[dat$A == 0] <- predict(model_0, newdata = dat %>% filter(A == 0),
                               type = "response")
  dat$S[dat$A == 1] <- predict(model_1, newdata = dat %>% filter(A == 1),
                               type = "response")
  
  # prepare main data
  dat$C <- ifelse(dat$S > threshold, 1, 0)
  labeled   <- dat %>% filter(!is.na(Y_miss))
  # unlabeled <- dat %>% filter(is.na(Y_miss))  # not directly used below
  
  # supervised on labeled only
  sup <- Audit_Fairness(
    Y = labeled$Y, S = labeled$S, A = labeled$A,
    threshold = threshold,
    method = "supervised"
  )
  
  # semi-supervised (S)
  #ss_s <- Audit_Fairness(
  #  Y = dat$Y_miss, S = dat$S, A = dat$A,
  #  threshold = threshold,
  #  method = "semi-supervised",
  #  basis = "Poly(S)"
  #)
  
  ss_s <- ImputeQuality(Y = dat$Y_miss, S = dat$S, A = dat$A, basis = "Poly(S)")
  
  # semi-supervised (S, W)
  #ss_sw <- Audit_Fairness(
  #  Y = dat$Y_miss, S = dat$S, A = dat$A,
  #  threshold = threshold,
  #  method = "semi-supervised",
  #  X = dat %>% select(contains("W")) %>% as.matrix(),
  #  basis = "Poly(S) + X"
  #)
  
  ss_sw <- ImputeQuality(Y = dat$Y_miss, S = dat$S, A = dat$A, X = dat %>% select(contains("W")), basis = "Poly(S) + X")
  
  # Beta calibration
  ss_Beta <- tryCatch(
  {
    Audit_Fairness(
      Y = dat$Y_miss,
      S = dat$S,
      A = dat$A,
      basis = "Beta"
    )
  },
  error = function(e) {
    NA
  }
)
  
  add_X <- dat %>% select(contains("W")) %>% as.matrix()
  add_X_S <- dat$S * add_X # interaction terms
  add_X_all <- cbind(add_X, add_X_S)
  
  #ss_sw_int <- Audit_Fairness(
  #  Y = dat$Y_miss, S = dat$S, A = dat$A,
  #  threshold = threshold,
  #  method = "semi-supervised",
  #  X = add_X_all,
  #  basis = "Poly(S) + X"
  #)
  ss_sw_int <- ImputeQuality(Y = dat$Y_miss, S = dat$S, A = dat$A, X = add_X_all, basis = "Poly(S) + X")
  
  
  list(
    sup       = sup,
    ss_s      = ss_s,
    ss_sw     = ss_sw,
    ss_Beta   = ss_Beta,
    ss_sw_int = ss_sw_int)
    #ss_sw2     = ss_sw2,
    #ss_sw_int2 = ss_sw_int2)
}

# ---------- Parallel execution (Mac/Linux)
mc_cores <- max(1, parallel::detectCores())

result <- mclapply(
  X = seq_len(nsim),
  FUN = sim_one,
  n = n, N = N, prev = prev, model = model, rho = rho, threshold = threshold,
  model_0 = model_0, model_1 = model_1,
  mc.cores = mc_cores
)

end_time <- Sys.time()
runtime <- difftime(end_time, start_time, units = "mins")
cat("Total runtime:", round(runtime, 2), "minutes\n")
saveRDS(result, file = paste0(model, "_n=",n,".rds"))