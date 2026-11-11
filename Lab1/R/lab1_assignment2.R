# Lab 1, Assignment 2 - Linear regression and ridge regression
#
# Sourced by lab1_assignment2.qmd. Paths are relative to Lab1/.
# Prefix objects with a2_ so they don't clash with other assignments'
# scripts when everything is sourced into the combined report.

# Load packages ---------------------------------------------------------------
require(tidyverse)
require(caret)

# read data -------------------------------------------------------------------

a2_data <- read.csv("data/parkinsons.csv", check.names = FALSE)

# 1. Train/test split (60/40) and scaling -------------------------------------

# keep only the target (motor_UPDRS) and the 16 predictors;
# drop variables subject#, age, sex, test_time and total_UPDRS
a2_voice_vars <- c("Jitter(%)", "Jitter(Abs)", "Jitter:RAP", "Jitter:PPQ5",
                   "Jitter:DDP", "Shimmer", "Shimmer(dB)", "Shimmer:APQ3",
                   "Shimmer:APQ5", "Shimmer:APQ11", "Shimmer:DDA",
                   "NHR", "HNR", "RPDE", "DFA", "PPE")
a2_data <- a2_data[, c("motor_UPDRS", a2_voice_vars)]

# partition 60/40 (code from: Lecture 1a, slide 34)
n <- dim(a2_data)[1]
set.seed(12345)
id <- sample(1:n, floor(n * 0.6))
a2_train_raw <- a2_data[id, ]
a2_test_raw  <- a2_data[-id, ]

# scale with the TRAINING means and sds, also for the test data, so that no
# information from the test set leaks into the model.
#
# motor_UPDRS is scaled too (centred at 0), which is why no intercept is needed.
a2_means <- colMeans(a2_train_raw)
a2_sds   <- map_dbl(a2_train_raw, sd)

a2_train <- as.data.frame(scale(a2_train_raw, center = a2_means, scale = a2_sds))
a2_test  <- as.data.frame(scale(a2_test_raw,  center = a2_means, scale = a2_sds))


# 2. Linear regression --------------------------------------------------------

# linear regression on all 16 voice variables
# without intercept as it is scaled and screws with ridge penalty
a2_lm <- lm(motor_UPDRS ~ . - 1, data = a2_train)
a2_lm_summary <- summary(a2_lm)

# coefficient table
a2_lm_coefs <- as.data.frame(a2_lm_summary$coefficients)

# variables that are significant at the 5% level
a2_lm_signif <- rownames(a2_lm_coefs)[a2_lm_coefs[, "Pr(>|t|)"] < 0.05]

# MSE = mean squared diff between observed and predicted values
## MSE function
a2_mse <- function(y, y_hat) mean((y - y_hat)^2)

## Calculating MSE
a2_lm_train_mse <- a2_mse(a2_train$motor_UPDRS,
                          predict(a2_lm, newdata = a2_train))
a2_lm_test_mse  <- a2_mse(a2_test$motor_UPDRS,
                          predict(a2_lm, newdata = a2_test))


# 3. Functions (base R only, no external packages) ----------------------------

# training data as matrix/vector, used by all functions in step 3
a2_X_train <- as.matrix(a2_train[, a2_voice_vars])   # n x 16 feature matrix
a2_y_train <- a2_train$motor_UPDRS                   # length-n target vector

# 3a. log P(T | theta, sigma) for the training data (T)
## This assumes that the error is Normal
a2_loglikelihood  <- function(theta, sigma) {
  n <- length(a2_y_train)
  residuals <- a2_y_train - a2_X_train %*% theta
  # log liklihood for normal distrobution
  -n / 2 * log(2 * pi * sigma^2) - sum(residuals^2) / (2 * sigma^2)
}

# 3b. minus log-likelihood + lambda * ||theta||^2  (obligatory oral defence)
a2_ridge <- function(theta, sigma, lambda) {
  neg_loglik <- -a2_loglikelihood(theta,sigma)
  penalty <- lambda * sum(theta^2)
  neg_loglik + penalty
}

# 3c. optimal theta and sigma for a given lambda, using optim(method = "BFGS")
#
# optim() minimises a function of ONE parameter vector,
# So theta (16 values) and sigma are packed together
# as par = c(theta, log(sigma)). One vector.
a2_ridge_opt <- function(lambda) {
  # numeber of paramaters
  p <- ncol(a2_X_train)

  # objective -- function to use in optim (a function to be minimized)
  # Unpacks par and evaluate the ridge function from 3b
  objective <- function(par) {
    theta <- par[1:p]
    sigma <- exp(par[p + 1])
    # returns minus log-likelihood of the training data + ridge penalty
    # (the value optim minimises)
    a2_ridge(theta, sigma, lambda)
  }

  # starting values: theta = 0, sigma = 1 (log(1) = 0)
  start <- c(rep(0, p), 0)

  fit <- optim(par = start, fn = objective, method = "BFGS")

  theta_hat <- fit$par[1:p]
  names(theta_hat) <- colnames(a2_X_train)

  list(theta       = theta_hat,
       sigma       = exp(fit$par[p + 1]),
       value       = fit$value,        # minimised ridge objective
       convergence = fit$convergence)  # 0 = optim converged
}

# 3d. degrees of freedom of the ridge model for a given lambda
a2_df <- function(lambda) {
  X <- a2_X_train
  p <- ncol(X)
  # Ridge hat matrix (smoother matrix) (Lecture 1d, slide 22)
  P <- X %*% solve(t(X) %*% X + lambda * diag(p)) %*% t(X)
  # df_eff = tr(P) trace of a matrix: the som of it's diagonal entries
  # (Lecture 1d, slide 17)
  sum(diag(P))
}


# 4. Ridge for lambda = 1, 100, 1000 ------------------------------------------

# TODO: optimal parameters for each lambda
# TODO: training and test MSE for each lambda
# TODO: degrees of freedom for each lambda
