library(dplyr)
library(tidyr)
library(ggplot2)

result <- readRDS("Data/scenario 1.rds")

# Oracle =================================
oracle_est <- do.call(rbind, lapply(result, function(ll) ll$oracle$est)) %>%
  group_by(Metric) %>%
  summarise_all(function(x) mean(x, na.rm = TRUE)) %>%
  pivot_longer(
    cols = -c(Metric),
    names_to = "Group",
    values_to = "Est"
  )

oracle_avar <- do.call(rbind, lapply(result, function(ll) ll$oracle$var)) %>%
  group_by(Metric) %>%
  summarise_all(function(x) mean(x, na.rm = TRUE)) %>%
  pivot_longer(
    cols = -c(Metric),
    names_to = "Group",
    values_to = "aVar"
  )

oracle <- oracle_est %>%
  left_join(oracle_avar, by = c("Metric", "Group"))


# Loop over methods =================================
methods <- names(result[[1]])[-1]

results_list <- list()

# Loop over each method to compute bias and MSE
for (method in methods) {
  
  # Create the estimation part (mean)
  est <- do.call(rbind, lapply(result, function(ll) ll[[method]]$est)) %>%
    group_by(Metric) %>%
    summarise_all(function(x) mean(x, na.rm = TRUE)) %>%
    pivot_longer(
      cols = -c(Metric),
      names_to = "Group",
      values_to = "Est"
    )
  
  # Create the variance part (variance)
  avar <- do.call(rbind, lapply(result, function(ll) ll[[method]]$var)) %>%
    group_by(Metric) %>%
    summarise_all(function(x) mean(x, na.rm = TRUE)) %>%
    pivot_longer(
      cols = -c(Metric),
      names_to = "Group",
      values_to = "aVar"
    )
  
  evar <- do.call(rbind, lapply(result, function(ll) ll[[method]]$est)) %>%
    group_by(Metric) %>%
    summarise_all(function(x) var(x, na.rm = TRUE)) %>%
    pivot_longer(
      cols = -c(Metric),
      names_to = "Group",
      values_to = "eVar"
    )
  
  # Combine estimation and variance
  combined <- est %>%
    left_join(avar, by = c("Metric", "Group")) %>%
    left_join(evar, by = c("Metric", "Group")) %>%
    mutate(Method = method)
  
  # Calculate the bias and MSE by comparing with oracle
  combined <- combined %>%
    left_join(oracle %>% select(Metric, Group, Est) %>% rename(Oracle_Est = Est), 
              by = c("Metric", "Group")) %>%
    mutate(bias = Est - Oracle_Est, MSE = bias^2 + eVar)
  
  # Append to the results list
  results_list[[method]] <- combined
}

# Combine all the results into one data frame
final_results <- bind_rows(results_list)

# Define the methods for comparison, excluding 'oracle' and 'sup'
methods <- methods[!methods %in% c("oracle", "sup")]

labels <- c("Infairness(S and W)", "Infairness(S only)", "Ji et al. (2020)")

# Filter the MSE from sup_est as the baseline
sup_mse <- final_results %>%
  filter(Method == "sup") %>%
  select(Metric, Group, MSE) %>%
  rename(sup_MSE = MSE)

# Initialize an empty data frame to store RE results
re_final <- data.frame()

# Loop over each method to compute RE relative to sup_est
for (i in seq_along(methods)) {
  
  # Get the current method and label
  method <- methods[i]
  label <- labels[i]
  
  # Get MSE of the current method
  method_mse <- final_results %>%
    filter(Method == method) %>%
    select(Metric, Group, MSE)
  
  # Compute RE (sup_MSE / method_MSE)
  re_temp <- sup_mse %>%
    left_join(method_mse, by = c("Metric", "Group")) %>%
    mutate(RE = sup_MSE / MSE, 
           Method = label)  # Apply the label
  
  # Append to the re_final data frame
  re_final <- rbind(re_final, re_temp)
}

# Convert 'Method' to a factor with the correct order for plotting, if needed
re_final$Method <- factor(re_final$Method, 
                          levels = c("Infairness(S only)", 
                                     "Infairness(S and W)", 
                                     "Ji et al. (2020)"))

final_results$Method <- factor(
  final_results$Method,
  levels = c("sup", "ss_poly", "ss", "beta"),
  labels = c(
    "Supervised",
    "Infairness(S only)",
    "Infairness(S and W)",  # Bold W using LaTeX syntax
    "Ji et al. (2020)"
  ))
plot_data <- final_results %>%
  filter(Group == "Delta") %>%
  filter(!Metric %in% c("TNR", "FNR")) %>%
  mutate(
    CI_lower = Est - 1.96 * sqrt(eVar),
    CI_upper = Est + 1.96 * sqrt(eVar),
    Metric_num = as.numeric(factor(Metric))  # numeric x for drawing segments
  )

oracle_df <- plot_data %>%
  distinct(Metric, Oracle_Est, Metric_num) %>%
  mutate(
    x_start = Metric_num - 0.45,
    x_end   = Metric_num + 0.45
  )

p1 <- ggplot(plot_data, aes(x = factor(Metric_num), y = Est, color = Method)) +
  geom_point(position = position_dodge2(width = 1), size = 2) +
  geom_segment(data = oracle_df,
               aes(x = x_start, xend = x_end,
                   y = Oracle_Est, yend = Oracle_Est),
               inherit.aes = FALSE,
               color = "black") +
  scale_x_discrete(
    labels = parse(text = paste0("Delta[", oracle_df$Metric, "]"))
  ) +
  ylab("Estimated Value") +
  xlab("") +
  ggsci::scale_color_nejm() +
  theme_bw() +
  theme(legend.position = "none")

p2 <- re_final %>%
  filter(!Metric %in% c("TNR", "FNR")) %>%
  filter(!Method == "Ji et al. (2020)") %>%
  filter(Group == "Delta") %>%
  mutate(Metric = case_when(
    Metric == "ACC" ~ "Delta[ACC]",
    Metric == "BS"  ~ "Delta[BS]",
    Metric == "F1"  ~ "Delta[F1]",
    Metric == "FNR" ~ "Delta[FNR]",
    Metric == "FPR" ~ "Delta[FPR]",
    Metric == "NPV" ~ "Delta[NPV]",
    Metric == "PPV" ~ "Delta[PPV]",
    Metric == "TNR" ~ "Delta[TNR]",
    Metric == "TPR" ~ "Delta[TPR]")) %>%
  ggplot(aes(x = Metric, y = RE, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge2()) +
  theme_bw() +
  geom_abline(intercept = 1, slope = 0, linetype = "dashed", col = "red") +
  ylab("Relative Efficiency") +
  xlab("") +
  scale_x_discrete(labels = scales::parse_format())+
  scale_fill_manual(values = c("#0072B5FF","#E18727FF")) +
  theme(legend.position = "none") + 
  labs(y = "Relative Efficiency\n Supervised : Infairness")