# Importing data into R and checking it
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(mvtnorm)

churn_data <- read_csv("C:/Users/franc/Downloads/Data_r.csv")
head(churn_data)
str(churn_data)

# Subset the data to include only relevant variables
selected_vars <- churn_data %>%
  select(gender, SeniorCitizen, Dependents, PaymentMethod)

# Create cross-tabulations between pairs of variables
for (var1 in colnames(selected_vars)) {
  for (var2 in colnames(selected_vars)) {
    if (var1 != var2) {
      cat("\nCross-tabulation between", var1, "and", var2, ":\n")
      print(table(selected_vars[[var1]], selected_vars[[var2]]))
    }
  }
}


# Subset the data to include only relevant variables
selected_vars <- churn_data %>%
  select(gender, SeniorCitizen, Dependents)

# Create cross-tabulation between pairs of variables
cross_tabs <- lapply(names(selected_vars), function(var1) {
  lapply(names(selected_vars), function(var2) {
    if (var1 != var2) {
      tbl <- table(selected_vars[[var1]], selected_vars[[var2]])
      df <- as.data.frame(tbl)
      colnames(df) <- c("Variable", "Other_Variable", "Freq")
      df$Variable <- as.character(df$Variable)
      df$Other_Variable <- as.character(df$Other_Variable)
      df
    } else {
      NULL
    }
  })
}) %>%
  do.call(rbind, .) %>%
  bind_rows(.id = "ID")

# Filter out NULL rows
cross_tabs <- cross_tabs[!is.na(cross_tabs$ID),]

# Convert to proportions
cross_tabs_prop <- cross_tabs %>%
  group_by(Variable) %>%
  mutate(Freq = Freq / sum(Freq))

# Plot heatmap
ggplot(cross_tabs_prop, aes(x = Variable, y = Other_Variable, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Cross-tabulation Heatmap",
       x = "Variable",
       y = "Other Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Checking for outliers in numerical variables
boxplot(churn_data$tenure)$out
boxplot(churn_data$MonthlyCharges)$out
boxplot(churn_data$TotalCharges)$out

# There are no outliers in this dataset

# Convert SeniorCitizen to factor with levels 'YES' and 'NO'
churn_data$SeniorCitizen <- as.factor(ifelse(churn_data$SeniorCitizen == 1, 'YES', 'NO'))

# Replacing "no internet service" and "no phone service" with "no"
library(stringr)
churn_data$MultipleLines <- str_replace(churn_data$MultipleLines, "No phone service", "No")
churn_data$OnlineSecurity <- str_replace(churn_data$OnlineSecurity, "No internet service", "No")

# Function to perform variable selection based on significance tests
variable_selection <- function(data, response) {
  selected <- c()
  
  for (variable in names(data)) {
    if (is.factor(data[[variable]])) {
      # Chi-square test for independence for categorical variables
      contingency_table <- table(data[[variable]], data[[response]])
      chi_squared <- chisq.test(contingency_table)$p.value
      
      if (chi_squared < 0.05) {
        selected <- c(selected, variable)
      }
    } else if (is.numeric(data[[variable]])) {
      # T-test for continuous variables
      t_test <- t.test(data[[variable]] ~ data[[response]])$p.value
      
      if (t_test < 0.05) {
        selected <- c(selected, variable)
      }
    }
  }
  
  return(selected)
}

# Perform variable selection
selected_variables <- variable_selection(churn_data, "Churn")
selected_variables

# Visualization
library(ggplot2)
library(gridExtra)
library(dplyr)

plot1 <- ggplot(churn_data, aes(x = SeniorCitizen)) + geom_bar(fill = "skyblue", color = "black") + labs(title = "Senior Citizen Distribution", x = "Senior Citizen", y = "Count")
plot2 <- ggplot(churn_data, aes(x = TotalCharges)) + geom_density(fill = "skyblue", color = "black") + labs(title = "Total Charges Distribution", x = "Total Charges", y = "Density")
plot3 <- ggplot(churn_data, aes(x = MonthlyCharges)) + geom_density(fill = "skyblue", color = "black") + labs(title = "Monthly Charges Distribution", x = "Monthly Charges", y = "Density")
plot4 <- ggplot(churn_data, aes(x = Churn)) + geom_bar(fill = "skyblue", color = "black") + labs(title = "Churn Distribution", x = "Churn", y = "Count")

churn_data <- mutate(churn_data, tenure_bin = tenure)

churn_data$tenure_bin[churn_data$tenure_bin >= 0 & churn_data$tenure_bin <= 24] <- '0-2 year'
churn_data$tenure_bin[churn_data$tenure_bin > 24 & churn_data$tenure_bin <= 48] <- '2-4 years'
churn_data$tenure_bin[churn_data$tenure_bin > 48 & churn_data$tenure_bin <= 72] <- '4-6 years'

churn_data$tenure_bin <- as.factor(churn_data$tenure_bin)

# Create a bar plot of the distribution of tenure_bin
plot5 <- ggplot(data = churn_data, aes(x = tenure_bin)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Tenure Bins", x = "Tenure Bin", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 3)

# Working on the numeric data, preparing numeric data from a dataset for analysis
num_columns <- c("MonthlyCharges", "TotalCharges")
churn_data[num_columns] <- sapply(churn_data[num_columns], as.numeric)

# Create a new data frame with only the selected numeric columns
telco_int <- churn_data[, c("MonthlyCharges", "TotalCharges")]

# Scale the selected numeric columns
telco_int_scaled <- as.data.frame(scale(telco_int))

# Selecting all columns from churn_data dataset except for columns 1, 6, 19, and 20
telco_cat <- churn_data[, -c(1:2, 4:20)]
telco_cat_new <- na.omit(telco_cat)

# Creating Dummy Variables
dummy_cont <- data.frame(sapply(telco_cat_new, function(x) data.frame(model.matrix(~x - 1, data = telco_cat_new))[,-1]))

# Final Dataset
telco_final <- cbind(telco_int_scaled, dummy_cont)

# Removing missing values
telco_final1 <- na.omit(telco_final)

# Correlation matrix
correlation_matrix <- cor(telco_final1)
correlation_matrix

# Visualization the correlation matrix using a heatmap
library(ggplot2)
library(reshape2)  # For melting the correlation matrix
melted_correlation <- melt(correlation_matrix)

# Plot the heatmap
ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()



#Necessary packages for Bayesian logistics
library(caret)
set.seed(123)
library(BayesLogit)


# Split the data into 50% training and 50% testing
train_index <- createDataPartition(telco_final1$Churn, p = 0.5, list = FALSE)
train_data <- telco_final1[train_index, ]
test_data <- telco_final1[-train_index, ]

# Extracting variables from the data
y_train <- train_data$Churn
X_train <- as.matrix(train_data[, -which(names(train_data) == "Churn")])
X_train <- cbind(rep(1, nrow(X_train)), X_train)

y_test <- test_data$Churn
X_test <- as.matrix(test_data[, -which(names(test_data) == "Churn")])
X_test <- cbind(rep(1, nrow(X_test)), X_test)

# Logistic regression model fitting
p <- dim(X_train)[2]
n <- length(y_train)
prior.mu <- rep(0, p)
prior.sigma <- rep(100, p)
beta <- rep(0, p)
var.prop1 <- 0.01 * diag(p)

sample.beta.metropolis <- function(beta) {
  beta.star <- t(rmvnorm(1, beta, var.prop1))
  eta.star <- exp(X_train %*% beta.star)
  p.star <- eta.star / (1 + eta.star)
  eta.beta <- exp(X_train %*% beta)
  p.beta <- eta.beta / (1 + eta.beta)
  log.r <- sum(dbinom(y_train, 1, p.star, log = TRUE)) - sum(dbinom(y_train, 1, p.beta, log = TRUE)) + sum(dnorm(beta.star, prior.mu, prior.sigma, log = TRUE)) - sum(dnorm(beta, prior.mu, prior.sigma, log = TRUE))
  u <- runif(1)
  if (log(u) < log.r) {
    beta <- beta.star
  }
  return(beta)
}

S <- 100000
burn <- 10000
beta.samples <- matrix(0, S - burn, p)

for (i in 1:S) {
  beta <- sample.beta.metropolis(beta)
  if (i > burn) {
    beta.samples[i - burn, ] <- beta
  }
  print(i)
}

#Bayesian estimate
Bay_estimate <- apply(beta.samples, 2, mean)

# Frequentist Regression
Freq_estimate <- coef(glm(y_train ~ X_train - 1, family = binomial))

# Checking for value count to see if data is imbalanced
value_counts <- table(telco_final1$Churn)
value_counts
churn_proportion <- prop.table(value_counts)
churn_proportion

# Confusion Matrix
beta_mean <- colMeans(beta.samples)
y_pred_bayeslogit_probs <- exp(X_test %*% beta_mean) / (1 + exp(X_test %*% beta_mean))
y_pred_bayeslogit <- ifelse(y_pred_bayeslogit_probs > 0.5, 1, 0)
y_test <- as.numeric(y_test)
y_pred_bayeslogit <- as.numeric(y_pred_bayeslogit)
confusion_bayeslogit <- confusionMatrix(factor(y_pred_bayeslogit, levels = c(0, 1)), factor(y_test, levels = c(0, 1)))

# Confusion matrix for glm
glm_model <- glm(y_train ~ X_train - 1, family = binomial)
y_pred_glm_probs <- predict(glm_model, newdata = data.frame(X_test), type = "response")
y_pred_glm <- ifelse(y_pred_glm_probs > 0.5, 1, 0)
confusion_glm <- confusionMatrix(factor(y_pred_glm, levels = c(0, 1)), factor(y_test, levels = c(0, 1)))

# Compare performance metrics
accuracy_bayeslogit <- confusion_bayeslogit$overall['Accuracy']
accuracy_glm <- confusion_glm$overall['Accuracy']
precision_bayeslogit <- confusion_bayeslogit$byClass['Pos Pred Value']
precision_glm <- confusion_glm$byClass['Pos Pred Value']
recall_bayeslogit <- confusion_bayeslogit$byClass['Sensitivity']
recall_glm <- confusion_glm$byClass['Sensitivity']
f1_score_bayeslogit <- confusion_bayeslogit$byClass['F1']
f1_score_glm <- confusion_glm$byClass['F1']

# Display the comparison
Results <- data.frame(
  Model = c("BayesLogit", "GLM"),
  Accuracy = c(accuracy_bayeslogit, accuracy_glm),
  Precision = c(precision_bayeslogit, precision_glm),
  Recall = c(recall_bayeslogit, recall_glm),
  F1_Score = c(f1_score_bayeslogit, f1_score_glm)
)

Results

# Credible intervals
beta_samples <- beta.samples
beta_means <- colMeans(beta_samples)
beta_sds <- apply(beta_samples, 2, sd)
lower_quantiles <- apply(beta_samples, 2, quantile, probs = 0.025)
upper_quantiles <- apply(beta_samples, 2, quantile, probs = 0.975)
credible_intervals <- data.frame(
  Coefficient = colnames(X_train),
  Lower_Quantile = lower_quantiles,
  Upper_Quantile = upper_quantiles
)

credible_intervals

# Calculate the mean and standard deviation for each coefficient
beta_means_glm <- coef(glm_model)
beta_sds_glm <- summary(glm_model)$coef[, "Std. Error"]
lower_quantiles_glm <- beta_means_glm - 1.96 * beta_sds_glm
upper_quantiles_glm <- beta_means_glm + 1.96 * beta_sds_glm
credible_intervals_glm <- data.frame(
  Coefficient = names(beta_means_glm),
  Lower_Quantile = lower_quantiles_glm,
  Upper_Quantile = upper_quantiles_glm
)

credible_intervals_glm

par(mfrow = c(3, 2))

#ACF for each parameter estimate
thin <- seq(20, nrow(beta_samples), by = 200)

for (i in 1:ncol(beta_samples)) {
  if (max(thin) <= nrow(beta_samples)) {
    acf(beta_samples[thin, i], main = paste0("ACF for Parameter ", colnames(beta_samples)[i]))
  } else {
    print("Error: thin sequence is out of range for rows in beta_samples.")
  }
}

par(mfrow = c(3, 2))

for (i in 1:ncol(beta_samples)) {
  if (max(thin) <= nrow(beta_samples)) {
    plot(beta_samples[-burn, i], type = "l", main = paste0("Trace for Parameter ", colnames(beta_samples)[i]))
  } else {
    print("Error: thin sequence is out of range for rows in beta_samples.")
  }
}

library(coda)

num_chains <- 4
chain_length <- nrow(beta.samples) / num_chains
chains_list <- list()

for (i in 1:num_chains) {
  start_index <- (i - 1) * chain_length + 1
  end_index <- i * chain_length
  chains_list[[i]] <- beta.samples[start_index:end_index, ]
}

mcmc_chains <- lapply(chains_list, mcmc)
gelman_rubin <- gelman.diag(mcmc_chains)
print(gelman_rubin)

par(mfrow = c(1,1))
library(pROC)
# Set the number of posterior samples to plot(since they are alot)
num_samples_to_plot <- 50

# Sample indices of posterior samples to plot
sample_indices <- sample(1:nrow(beta.samples), num_samples_to_plot)

# Calculate the ROC curve for each sampled posterior sample
roc_curves_Bay_estimate1 <- lapply(sample_indices, function(i) roc(y_test, plogis(X_test %*% beta.samples[i, ])))

# Calculate the ROC curve for posterior means
roc_curve_Bay_estimate <- roc(y_test, plogis(X_test %*% Bay_estimate))

# Plot individual ROC curves
plot(roc_curves_Bay_estimate1[[1]], col = "yellow", main = "ROC Curve - Bayesian Estimates")

for (i in 2:length(roc_curves_Bay_estimate1)) {
  plot(roc_curves_Bay_estimate1[[i]], col = "yellow", add = TRUE)
}

# Plot ROC curve for posterior means
plot(roc_curve_Bay_estimate, col = "blue", add = TRUE)

# Add legend
legend("bottomright", legend = c("Posterior Samples", "Posterior Means"), col = c("gray", "blue"), lty = 1, cex = 0.8)

# Calculate predicted probabilities for the GLM model
y_pred_glm_probs <- predict(glm_model, newdata = data.frame(X_test), type = "response")

# Calculate ROC curve for the GLM model
roc_curve_glm <- roc(y_test, y_pred_glm_probs)

# Plot ROC curve for the GLM model
plot(roc_curve_glm, col = "red", main = "ROC Curve - GLM")

auc_bayeslogit <- auc(roc_curve_Bay_estimate)
auc_fre_logit <- auc(roc_curve_glm)

# Extract feature names
feature_names <- colnames(X_train)

# Combine feature names with corresponding coefficient estimates
feature_coefficients <- data.frame(Feature = feature_names, Coefficient = Bay_estimate)

# Sort the coefficients by magnitude to identify the most influential features
feature_coefficients <- feature_coefficients[order(abs(feature_coefficients$Coefficient), decreasing = TRUE), ]

# Display the feature coefficients
print(feature_coefficients)


