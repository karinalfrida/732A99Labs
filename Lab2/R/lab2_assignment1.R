#Read data from file
data <- read.csv(r"(C:\Users\jonas\732A99 Machine Learning\732A99Labs\Lab2\data\tecator.csv)")

#Split data
n <- nrow(data)
set.seed(12345)
id <- sample(1:n, floor(n * 0.5))
train <- data[id, ]

id1 <- setdiff(1:n, id)
test <- data[id1, ]

# 1. ----------------------------------------------------------------------------
# Keep only channels and fat
vars <- c(paste0("Channel", 1:100), "Fat")
train_lm <- train[, vars]
test_lm <- test[, vars]

fit_lm <- lm(Fat ~ ., data = train_lm)

# Measure training and test error
pred_train <- predict(fit_lm, newdata = train_lm)
pred_test <- predict(fit_lm, newdata = test_lm)

mse_train <- mean((train_lm$Fat - pred_train)^2)
mse_test <- mean((test_lm$Fat - pred_test)^2)

mse_train # Gives MSE 0.005709117
mse_test  # Gives MSE 722.4294 

# Compared with just guessing
var(test_lm$Fat) # Gives MSE 167.3707
# Model heavily overfitted due to 100 parameters but only 107 test cases, also
# data greatly correlated, making theta-hat very unstable.


# 2. ---------------------------------------------------------------------------

# No code, only reporting the cost function to be optimized

# 3. ---------------------------------------------------------------------------
library(glmnet)

x_train <- as.matrix(train_lm[, paste0("Channel", 1:100)])
y_train <- train_lm$Fat

fit_lasso <- glmnet(x_train, y_train, alpha = 1, family = "gaussian")

plot(fit_lasso, xvar = "lambda", label = TRUE)

# Gives which lambdas give df == 3
fit_lasso$lambda[which(fit_lasso$df == 3)]
# gives: 0.8530452 0.7772630 0.7082131

# 4. ---------------------------------------------------------------------------
fit_ridge <- glmnet(x_train, y_train, alpha = 0, family = "gaussian")

plot(fit_ridge, xvar = "lambda", label = TRUE)

# 5. ---------------------------------------------------------------------------
# Cross-validation
set.seed(12345)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "gaussian")

plot(cv_lasso)

# Optimal lambda and number of variables
cv_lasso$lambda.min
log(cv_lasso$lambda.min)
cv_lasso$nzero[cv_lasso$lambda == cv_lasso$lambda.min]

# Predict test data with optimal lambda
x_test <- as.matrix(test_lm[, paste0("Channel", 1:100)])
y_test <- test_lm$Fat

pred_test_lasso <- predict(cv_lasso, newx = x_test, s = "lambda.min")
mse_test_lasso <- mean((y_test - pred_test_lasso)^2)
mse_test_lasso

# Scatter plot: true vs predicted
plot(y_test, pred_test_lasso,
     xlab = "True fat", ylab = "Predicted fat",
     main = "LASSO (optimal lambda): test data")
abline(0, 1, col = "red")


