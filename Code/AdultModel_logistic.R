library(dplyr)
library(tidyr)
library(doParallel)
library(glmnet)

dat <- data.table::fread("adult.data",
  na.strings = "?"
)
test <- data.table::fread("adult.test",
  na.strings = "?"
)
test$V15 <- test$V15 %>% gsub("\\.", "", .)

dat <- rbind(dat, test)

colnames(dat) <- c(
  "age", "workclass", "fnlwgt", "education",
  "education_num", "marital_status", "occupation",
  "relationship", "race", "sex", "capital_gain",
  "capital_loss", "hours_per_week", "native_country",
  "income_per_year"
)

dat <- dat %>% drop_na()

# Convert outcome into binary varviables
dat <- dat %>%
  mutate(
    income_per_year =
      ifelse(income_per_year == "<=50K", 0, 1),
    native_country =
      ifelse(native_country == "United-States", "US", "Non-US"),
    marital_status =
      ifelse(marital_status %in% c("Married-civ-spouse",
                                   "Married-spouse-absent",
                                   "Married-AF-spouse"), "Married", marital_status),
    workclass =
      ifelse(workclass %in% c("Federal-gov", "Local-gov", "State-gov"), "Gov", workclass),
    workclass =
      ifelse(workclass %in% c("Self-emp-inc", "Self-emp-not-inc"), "Self-Employed", workclass),
  ) %>%
  filter(workclass != "Without-pay") %>%
  mutate(occupation = ifelse(occupation %in% c("Adm-clerical", "Exec-managerial", "Prof-specialty", "Tech-support"), "White-Collar", occupation),
         occupation = ifelse(occupation %in% c("Craft-repair", "Farming-fishing", "Handlers-cleaners", "Machine-op-inspct", "Transport-moving"), "Blue-Collar", occupation),
         occupation = ifelse(occupation %in% c("Other-service", "Priv-house-serv", "Protective-serv", "Sales", "Armed-Forces"), "Service", occupation)) %>%
  select(-education)

set.seed(1234)
train_index <- sample(1:nrow(dat), 3000)

# Prepare the data
# Ensure categorical variables are factors
dat$workclass <- as.factor(dat$workclass)
#dat$education <- as.factor(dat$education)
dat$marital_status <- as.factor(dat$marital_status)
dat$occupation <- as.factor(dat$occupation)
dat$relationship <- as.factor(dat$relationship)
dat$race <- as.factor(dat$race)
dat$sex <- as.factor(dat$sex)
dat$native_country <- as.factor(dat$native_country)
dat$age <- scale(dat$age)
dat$fnlwgt <- scale(dat$fnlwgt)
dat$capital_gain <- scale(dat$capital_gain)
dat$capital_loss <- scale(dat$capital_loss)
dat$hours_per_week <- scale(dat$hours_per_week)

# Prepare the data
x <- model.matrix(factor(income_per_year) ~ . - 1, data = dat[train_index, ]) %>% as.matrix  # -1 to omit intercept
y <- dat[train_index, "income_per_year"]$income_per_year  # Assuming 'income_per_year' is already a factor
# Fit the logistic regression model with L1 penalty using cross-validation
cv_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1)  # alpha = 1 for L1 penalty

# Best lambda value
best_lambda <- cv_fit$lambda.min

# Fit the model using the best lambda
model <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)
# Prediction (probabilities of the positive class)
x_new <- model.matrix(~ . , data = dat[, -"income_per_year"])  %>% as.matrix  # Prepare new data matrix
yhat <- predict(model, newx = x_new, type = "response")[,1]  # Predict probabilities for the positive class

# # Random Forest Model
# rf <- randomForest::randomForest(
#   factor(income_per_year) ~ .,
#   data = dat[train_index, ]
# )
#
# # Prediction
# yhat <- predict(rf,newdata = dat, type = "prob")[,2]

cl <- makeCluster(60)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(Infairness)
  library(glmnet)
  library(dplyr)
})
clusterExport(cl, varlist = list("dat", "yhat", "train_index"))

results <- pbapply::pblapply(cl = cl, X = 1:1e4, FUN = function(j) {
  # Get the indices not in the training set
  remaining_indices <- setdiff(1:nrow(dat), train_index)

  # Sample 1000 indices from the remaining for the test set
  test_index <- sample(remaining_indices, 1000)

  # Oracle
  oracle <- SupervisedFairness(
    Y = dat$income_per_year[-train_index],
    S = yhat[-train_index],
    A = dat$sex[-train_index]
  )

  # Supervised
  sup <- SupervisedFairness(
    Y = dat$income_per_year[test_index],
    S = yhat[test_index],
    A = dat$sex[test_index]
  )
  
  # Infairness
  outcome_miss <- dat$income_per_year
  outcome_miss[-test_index] <- NA
  ss <- Infairness(
    Y = outcome_miss[-train_index],
    S = yhat[-train_index],
    A = dat$sex[-train_index]
  )
  
  list(oracle_est = oracle$est,
       oracle_var = oracle$var,
       sup_est = sup$est,
       sup_var = sup$var,
       inf_est = ss$est,
       inf_var = ss$var)
})
stopCluster(cl)

saveRDS(results, file = "adult_logistic_sex_n=1000.rds")