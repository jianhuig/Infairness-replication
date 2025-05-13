library(dplyr)
library(tidyr)
library(ggplot2)


result <- readRDS("Data/scenario 1.rds")


coverage_result <- c()
methods <- c("sup", "ss_poly", "ss")
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