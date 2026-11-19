# Lab 2, Assignment 3: Principal components and implicit regularization
#
# Data: communities.csv, crime level in US communities.
# Target: ViolentCrimesPerPop (violent crimes per 100K population).
# Variable descriptions:
# https://archive.ics.uci.edu/ml/datasets/Communities+and+Crime
#
# Paths are relative to the Lab2 folder (the RStudio/Quarto project root).
# Objects are prefixed a3_ so they don't clash with other assignment scripts.

library(tidyverse)
library(caret)

# read data
a3_communities_df <- read.csv("data/communities.csv")

# 1. PCA with eigen() ----------------------------------------------------------

# Scale (centre to mean 0, scale to sd 1) all variables except the target
a3_feature_cols_index <- names(a3_communities_df) != "ViolentCrimesPerPop"
a3_scaler_argument <- preProcess(a3_communities_df[a3_feature_cols_index])
a3_communities_scaled_df <- predict(a3_scaler_argument, a3_communities_df)

# eigen values from covariance matrix
a3_design_matrix <- a3_communities_scaled_df[a3_feature_cols_index]
a3_S <- cov(a3_design_matrix)
a3_e <- eigen(a3_S)

a3_cum <- cumsum(a3_e$values) / sum(a3_e$values)

# elbow (scree) plot: proportion of variance explained by each component
a3_scree_df <- tibble(
  component = seq_along(a3_e$values),
  prop_var  = a3_e$values / sum(a3_e$values)
)

a3_scree_plot <- ggplot(a3_scree_df, aes(x = component, y = prop_var)) +
  geom_line() +
  geom_point(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Principal component",
       y = "Proportion of variance explained") +
  theme_minimal()



# 2. PCA with princomp() -------------------------------------------------------


# 3. Linear regression, 50/50 train/test split ---------------------------------


# 4. Cost function, BFGS with optim() and early stopping -----------------------

