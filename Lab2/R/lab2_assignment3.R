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
a3_communities <- read.csv("data/communities.csv")

# 1. PCA with eigen() ----------------------------------------------------------

# Scale (centre to mean 0, scale to sd 1) all variables except the target
a3_feature_cols <- names(a3_communities) != "ViolentCrimesPerPop"
a3_scaler <- preProcess(a3_communities[a3_feature_cols])
a3_communities_scaled <- predict(a3_scaler, a3_communities)


# 2. PCA with princomp() -------------------------------------------------------


# 3. Linear regression, 50/50 train/test split ---------------------------------


# 4. Cost function, BFGS with optim() and early stopping -----------------------

