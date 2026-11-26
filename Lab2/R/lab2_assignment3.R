# Lab 2, Assignment 3: Principal components and implicit regularization

# library(tidyverse)
# library(caret)
# library(patchwork)

# read data
a3_communities_df <- read.csv("data/communities.csv")

# 1. PCA with eigen() ----------------------------------------------------------

# Scale (centre to mean 0, scale to sd 1) all variables except the target
a3_feature_cols_index <- names(a3_communities_df) != "ViolentCrimesPerPop"
a3_scaler_argument <- preProcess(a3_communities_df[a3_feature_cols_index])
a3_communities_scaled_df <- predict(a3_scaler_argument, a3_communities_df)

# covariance matrix S of the scaled features, then its eigenvalues
# (variance along each component) and eigenvectors (component directions)
a3_design_matrix <- a3_communities_scaled_df[a3_feature_cols_index]
a3_S <- cov(a3_design_matrix)
a3_e <- eigen(a3_S)


# variance explained, one row per principal component
a3_prop_df <- tibble(
  # component number k (eigen() sorts by eigenvalue, so 1 = largest)
  component = seq_along(a3_e$values),
  # proportion of the total variance explained by component k
  # (its eigenvalue divided by the sum of all eigenvalues)
  prop_var  = a3_e$values / sum(a3_e$values),
  # cumulative proportion explained by components 1 to k together
  cum_prop_var = cumsum(a3_e$values / sum(a3_e$values))
)

# number of components to capture at least 95% of the variance
a3_nr_comp_95 <- which(a3_prop_df$cum_prop_var >= 0.95)[1]

# Proportion of variance captured by 2 components
a3_prop_2_comp <- a3_prop_df$cum_prop_var[2]

# scree plot: proportion of variance explained by each component
a3_scree_plot <- ggplot(a3_prop_df, aes(x = component, y = prop_var)) +
  geom_line() +
  geom_point(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Principal component",
       y = "Proportion of variance explained") +
  theme_bw()

# cumulative proportion of variance explained, 95% threshold as a dashed line
a3_cum_var_plot <- ggplot(a3_prop_df, aes(x = component, y = cum_prop_var)) +
  geom_hline(yintercept = 0.95, colour = "grey50", linetype = "dashed") +
  geom_line() +
  geom_point(size = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Principal component",
       y = "Cumulative variance explained") +
  theme_bw()

## Report prep stuff

# scree plot and cumulative plot side by side, for the report
a3_var_plots <- a3_scree_plot | a3_cum_var_plot

# number of features (100)
a3_n_features <- sum(a3_feature_cols_index)

# "formats a proportion as a percentage" - function
a3_pct <- function(x, accuracy = 0.1) scales::percent(x, accuracy = accuracy)


# 2. PCA with princomp() -------------------------------------------------------

# PCA with princomp()
# ('prcomp()' does tha same but devides with (n-1) instead of n when computing sd^2)
a3_princomp <- princomp(a3_design_matrix)

# Extract loadings
a3_loadings <- a3_princomp$loadings

# Creating df for trace plot
# Loading of each feature on the first component
a3_pc1_loadings_df <- tibble(
  feature_index = seq_len(nrow(a3_loadings)),   # index
  feature       = rownames(a3_loadings),        # feature
  loading       = a3_loadings[, 1]              # loadings col 1
)

# Benchmark, if all features contibuted equally
# +-1/sqrt(nr of features) (= 0.1 here).
a3_pc1_benchmark <- 1 / sqrt(nrow(a3_loadings))

# trace plot
a3_pc1_trace_plot <- ggplot(a3_pc1_loadings_df, aes(x = feature_index, y = loading)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_hline(yintercept = c(-1, 1) * a3_pc1_benchmark,
             colour = "grey50", linetype = "dashed") +
  geom_segment(aes(xend = feature_index, y = 0, yend = loading), linewidth = 1) +
  labs(x = "Feature index",
       y = "PC1 loading") +
  theme_bw()

# top 5 features by absolute PC1 loading, with each feature's share of PC1
# (squared loading, the shares sum to 1 over all features) and cumulative share
a3_top5_features_PC1 <- a3_pc1_loadings_df |>
  select(-feature_index) |>
  mutate(abs_loading = abs(loading)) |>
  arrange(desc(abs_loading)) |>
  mutate(share     = loading^2,
         cum_share = cumsum(share)) |>
  head(5)
# NOTE: these are basically SES features

# Table for the report
a3_top5_table <- a3_top5_features_PC1 |>
  mutate(
    share     = 100 * share,
    # The 5 features correlation with target
    cor_crime = sapply(feature, function(f) {
      cor(a3_communities_df[[f]], a3_communities_df$ViolentCrimesPerPop)
    })
  ) |>
  select(feature, loading, share, cor_crime)

# ggplot of PC1 to PC2, colored by 'ViolentCrimesPerPop' (target)
a3_pc_scores_df <- tibble(
  PC1 = a3_princomp$scores[, 1],
  PC2 = a3_princomp$scores[, 2],
  ViolentCrimesPerPop = a3_communities_df$ViolentCrimesPerPop
)

a3_pc12_scores_plot <- ggplot(a3_pc_scores_df,
                              aes(x = PC1, y = PC2, colour = ViolentCrimesPerPop)) +
  # point formatting
  geom_point(size = 1.7, alpha = 0.8, shape = 16) +
  scale_colour_viridis_c() +
  # labels
  labs(x = paste0("PC1 (", scales::percent(a3_prop_df$prop_var[1], accuracy = 0.1), ")"),
       y = paste0("PC2 (", scales::percent(a3_prop_df$prop_var[2], accuracy = 0.1), ")"),
       colour = "ViolentCrimesPerPop") +
  theme_bw()

# correlation between each component's scores and the target
a3_cor_pc1_crime <- cor(a3_pc_scores_df$PC1, a3_pc_scores_df$ViolentCrimesPerPop)
a3_cor_pc2_crime <- cor(a3_pc_scores_df$PC2, a3_pc_scores_df$ViolentCrimesPerPop)

# top 5 features on PC2 (by absolute loading), to interpret the PC2 axis
a3_top5_features_PC2 <- tibble(
  feature = rownames(a3_loadings),
  loading = a3_loadings[, 2]
) |>
  arrange(desc(abs(loading))) |>
  head(5)




# 3. Linear regression, 50/50 train/test split ---------------------------------

# Split data 50/50 (Lecture 1a, slide 34.)
a3_n <- nrow(a3_communities_df)
set.seed(12345)
a3_train_id <- sample(1:a3_n, floor(a3_n * 0.5))
a3_train_df <- a3_communities_df[a3_train_id, ]
a3_test_df  <- a3_communities_df[-a3_train_id, ]

# Scale features and response with caret (Lecture 1d, slide 16)
a3_scaler_train    <- preProcess(a3_train_df)
a3_train_scaled_df <- predict(a3_scaler_train, a3_train_df)
a3_test_scaled_df  <- predict(a3_scaler_train, a3_test_df)

# Linear regression on the scaled training data
a3_lm <- lm(ViolentCrimesPerPop ~ ., data = a3_train_scaled_df)

# mean squared error
a3_mse <- function(y, y_hat) mean((y - y_hat)^2)

a3_y_test <- a3_test_scaled_df$ViolentCrimesPerPop
a3_test_pred <- predict(a3_lm, newdata = a3_test_scaled_df)

# training and test errors (MSE, on the scaled response)
a3_lm_train_mse <- a3_mse(a3_train_scaled_df$ViolentCrimesPerPop,
                          predict(a3_lm, newdata = a3_train_scaled_df))
a3_lm_test_mse  <- a3_mse(a3_y_test, a3_test_pred)

# training and test R2 values
a3_lm_train_R2 <- summary(a3_lm)$r.squared
a3_lm_test_R2 <- 1 - sum((a3_y_test - a3_test_pred)^2) / sum((a3_y_test - mean(a3_y_test))^2)


# 4. Cost function, BFGS with optim() and early stopping -----------------------

# features and response as matrices
a3_X_train <- as.matrix(a3_train_scaled_df[a3_feature_cols_index])
a3_y_train <- a3_train_scaled_df$ViolentCrimesPerPop
a3_X_test  <- as.matrix(a3_test_scaled_df[a3_feature_cols_index])
# (a3_y_test is defined in step 3)

# Training and test error for every evaluation of the cost function.
# Only the errors are stored, not the parameter vectors. (hint 1 instructions)
a3_train_errors <- c()
a3_test_errors  <- c()

# Objects to save best theta vector
a3_best_test_mse <- Inf
a3_best_theta <- NULL

# Cost function for linear regression without intercept:
# Training MSE as a function of the parameter vector theta.
a3_cost <- function(theta) {

  train_mse <- a3_mse(a3_y_train, a3_X_train %*% theta)
  test_mse  <- a3_mse(a3_y_test, a3_X_test %*% theta)

  # Record the training and test error for that theta into parent env. vectors
  a3_train_errors <<- c(a3_train_errors, train_mse)
  a3_test_errors  <<- c(a3_test_errors, test_mse)

  # Keep theta vector only when test MSE improves, save into parent object
  if (test_mse < a3_best_test_mse) {
    a3_best_test_mse <<- test_mse
    a3_best_theta <<- theta
  }

  # Our cost function slash objective
  return(train_mse)
}

# optim() to find the theta that make the cost function (MSE) as small as possible
a3_bfgs <- optim(par = rep(0, ncol(a3_X_train)),  # 100 zeros (nr of features)
                 fn = a3_cost,                    # cost function defined above
                 method = "BFGS",                 # optimization algorithm
                 control = list(
                   maxit = 1000
                   # trace = 1,
                   # REPORT = 10
                   )
                 )

# early stopping: the iteration with the lowest test error
a3_opt_iter     <- which.min(a3_test_errors)
a3_es_train_mse <- a3_train_errors[a3_opt_iter]
a3_es_test_mse  <- a3_test_errors[a3_opt_iter]

# errors against iteration number, first 500 iterations discarded (hint 2)
a3_es_df <- tibble(
  iteration = seq_along(a3_train_errors),
  Training  = a3_train_errors,
  Test      = a3_test_errors
) |>
  filter(iteration > 500) |>
  pivot_longer(c(Training, Test),
               names_to = "data",
               values_to = "mse") |>
  mutate(data = factor(data, levels = c("Training", "Test")))

# Plot of the MSE over Iterations in the optimizer
a3_es_plot <- ggplot(a3_es_df, aes(x = iteration, y = mse, colour = data)) +
  geom_line() +
  geom_vline(xintercept = a3_opt_iter, linetype = "dashed", colour = "grey40") +
  scale_x_log10(breaks = scales::breaks_log(n = 8),
                labels = scales::label_comma()) +
  annotation_logticks(sides = "b") +
  coord_cartesian(ylim = c(0.25, 0.7)) +
  labs(x = "Iteration (cost function evaluation, log scale)",
       y = "MSE",
       colour = NULL) +
  theme_bw() +
  theme(legend.position = "top")


