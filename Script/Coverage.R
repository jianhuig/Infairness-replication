library(dplyr)
library(tidyr)
library(ggplot2)


result <- readRDS("Data/scenario 31023.rds")

# Oracle =================================
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
model_1 <- glm(
  Y ~ ., family = binomial(),
  data = indep %>% filter(A == 1) %>% select(Y, contains("X"))
)
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

oracle <- oracle$est %>%
  pivot_longer(
    cols = -c(Metric),
    names_to = "Group",
    values_to = "Est"
  ) %>%
  data.frame()

coverage_result <- c()
methods <- c("sup", "ss_s", "ss_s", "ss_Beta", "ss_sw_int")

for (method in methods) {
  
  est <- do.call(rbind, lapply(result, function(ll) ll[[method]]$est)) %>%
    pivot_longer(
      cols = -c(Metric),
      names_to = "Group",
      values_to = "Est"
    )
  
  var <- do.call(rbind, lapply(result, function(ll) ll[[method]]$var)) %>%
    pivot_longer(
      cols = -c(Metric),
      names_to = "Group",
      values_to = "Var"
    )
  
  for(m in unique(est$Metric)){
    for(g in unique(est$Group)){
      est_temp <- est %>% filter(Metric == m, Group == g) %>% pull(Est)
      var_temp <- var %>% filter(Metric == m, Group == g) %>% pull(Var)
      
      coverage <- sum((est_temp - qnorm(0.975) * sqrt(var_temp) <= as.numeric(oracle[oracle$Metric == m & oracle$Group == g, "Est"])) &
                        (est_temp + qnorm(0.975) * sqrt(var_temp) >= as.numeric(oracle[oracle$Metric == m & oracle$Group == g, "Est"]))) / length(est_temp)
      
      coverage_result <- rbind(coverage_result, data.frame(Method = method, Metric = m, Group = g, Coverage = coverage))
    }
  }
  
}

coverage_result %>%
  filter(Group == "Delta") %>%
  select(Metric, Method, Coverage) %>%
  mutate(Method = factor(Method, levels = c("sup", "ss_poly", "ss"), 
                         labels = c("Supervised", "Infairness (S only)", "Infairness (S + W)"))) %>% 
  filter(!Metric %in% c("TNR", "FNR")) %>%
  pivot_wider(names_from = Method, values_from = Coverage) %>%
  arrange(Metric) %>%
  mutate(Metric = case_when(
    Metric == "TPR" ~ "$\\Delta_{\\mathrm{TPR}}$",
    Metric == "TNR" ~ "$\\Delta_{\\mathrm{TNR}}$",
    Metric == "FPR" ~ "$\\Delta_{\\mathrm{FPR}}$",
    Metric == "FNR" ~ "$\\Delta_{\\mathrm{FNR}}$",
    Metric == "NPV" ~ "$\\Delta_{\\mathrm{NPV}}$",
    Metric == "PPV" ~ "$\\Delta_{\\mathrm{PPV}}$",
    Metric == "ACC" ~ "$\\Delta_{\\mathrm{ACC}}$",
    Metric == "F1"  ~ "$\\Delta_{\\mathrm{F1}}$",
    Metric == "BS"  ~ "$\\Delta_{\\mathrm{BS}}$",
    TRUE ~ Metric
  )) %>%
  kableExtra::kable(
    format = "latex",
    booktabs = TRUE,
    digits = 2,
    escape = FALSE, # This is important to render LaTeX math notation
    caption = "Coverage for Delta"
  )