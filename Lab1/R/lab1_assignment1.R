library(kknn)

# 1. + 2.-----------------------------------------------------------------------
# Read and separate data
a1_data <- read.csv("data/optdigits.csv", header = FALSE)
a1_data$V65 <- as.factor(a1_data$V65)
a1_n = dim(a1_data)[1]
set.seed(12345)
a1_id = sample(1:a1_n, floor(a1_n*0.5))
a1_train = a1_data[a1_id, ]

a1_id1 = setdiff(1:a1_n, a1_id)
set.seed(12345)
a1_id2 = sample(a1_id1, floor(a1_n*0.25))
a1_valid = a1_data[a1_id2, ]

a1_id3 = setdiff(a1_id1, a1_id2)
a1_test = a1_data[a1_id3, ]
#-------------------------------------------------------------------------------

a1_test_kknn <- kknn(
  V65 ~ .,
  train = a1_train,
  test = a1_test,
  k = 30,
  kernel = "rectangular"
)

a1_train_kknn <- kknn(V65 ~ .,
                   train = a1_train,
                   test = a1_train,
                   k = 30,
                   kernel = "rectangular"
                   )

a1_cm_test <- table(true = a1_test$V65, predicted = fitted(a1_test_kknn))
a1_n_errors_test <- nrow(a1_test) - sum(diag(a1_cm_test))
a1_misclass_rate_test <- a1_n_errors_test/nrow(a1_test)

a1_cm_train <- table(true = a1_train$V65, predicted = fitted(a1_train_kknn))
a1_n_errors_train <- nrow(a1_train) - sum(diag(a1_cm_train))
a1_misclass_rate_train <- a1_n_errors_train/nrow(a1_train)

#Comment: The 30-NN classifier has misclassification rates of 4.5% (training)
#and 5.3% (test), so about 95% of the digits are classified correctly. That
#is good quality overall, and the small gap between the two errors suggests
#little overfitting.

#Digits 0, 2, 6, 7 are classified almost perfectly. The hardest digits are
#4, 5, 8. Common confusions are 4 -> 7/9, 8 -> 1 and 5 -> 9, pairs that can
#look alike in the low-resolution 8x8 images.

# 3.----------------------------------------------------------------------------

# Row # in train for all real 8's
a1_index8 <- which(a1_train$V65 == "8")

# Probability for the class "8" for just those rows
a1_prob8 <- a1_train_kknn$prob[a1_index8, "8"]

# Sort from lowest to highest probability and translate to rownumber in train
a1_index8_sorted <- a1_index8[order(a1_prob8)]

a1_hardest <- head(a1_index8_sorted, 3) # 3 lowest probabilities
a1_easiest <- tail(a1_index8_sorted, 2) # 2 highest probabilities

# Make row in train to 8x8-matrix and plot it
a1_plot_digit <- function(row, title){
  m <- matrix(as.numeric(a1_train[row, 1:64]), nrow = 8, ncol = 8, byrow = TRUE)
  m <- m[8:1, ] # turn rows, heatmap plots first row on the bottom
  heatmap(m, Colv = NA, Rowv = NA, labRow = NA, labCol = NA,
          margins = c(0.5, 0.5))
}

# The easy ones are easy to tell they are 8's and the hard ones are difficult.
# 2 of the hard ones seem more likely to me to be ones and one of them could
# just as well be a six

# 4.----------------------------------------------------------------------------

a1_misclass_train <- numeric(30)
a1_misclass_valid <- numeric(30)

for (i in 1:30){
  train_kknn_inner <- kknn(V65 ~ .,
                     train = a1_train,
                     test = a1_train,
                     k = i,
                     kernel = "rectangular"
  )
  valid_kknn_inner <- kknn(V65 ~ .,
                     train = a1_train,
                     test = a1_valid,
                     k = i,
                     kernel = "rectangular"
  )

  cm_train_inner <- table(true = a1_train$V65, predicted = fitted(train_kknn_inner))
  n_errors_train_inner <- nrow(a1_train) - sum(diag(cm_train_inner))
  misclass_rate_train_inner <- n_errors_train_inner/nrow(a1_train)

  cm_valid_inner <- table(true = a1_valid$V65, predicted = fitted(valid_kknn_inner))
  n_errors_valid_inner <- nrow(a1_valid) - sum(diag(cm_valid_inner))
  misclass_rate_valid_inner <- n_errors_valid_inner/nrow(a1_valid)

  a1_misclass_train[i] <- misclass_rate_train_inner
  a1_misclass_valid[i] <- misclass_rate_valid_inner
}

# Plot training and validation misclassification errors against K
a1_plot_misclass <- function(){
  plot(1:30, a1_misclass_train, type = "b", col = "blue", pch = 19,
       ylim = range(c(a1_misclass_train, a1_misclass_valid)),
       xlab = "K", ylab = "Misclassification error")
  lines(1:30, a1_misclass_valid, type = "b", col = "red", pch = 19)
  legend("bottomright", legend = c("Training", "Validation"),
         col = c("blue", "red"), pch = 19, lty = 1)
}

# Optimal K = lowest validation error
a1_k_opt <- which.min(a1_misclass_valid)
a1_k_opt

# test error for optimal K
a1_test_kknn_opt <- kknn(V65 ~ .,
                      train = a1_train,
                      test = a1_test,
                      k = a1_k_opt,
                      kernel = "rectangular")

a1_cm_test_opt <- table(true = a1_test$V65, predicted = fitted(a1_test_kknn_opt))
a1_misclass_test_opt <- (nrow(a1_test) - sum(diag(a1_cm_test_opt))) / nrow(a1_test)

# Compare the three errors for optimal K
c(train = a1_misclass_train[a1_k_opt],
  valid = a1_misclass_valid[a1_k_opt],
  test = a1_misclass_test_opt)

#k_opt = 3.
# train      valid       test
# 0.01151230 0.02513089 0.02403344

# 5.----------------------------------------------------------------------------
a1_ce_valid <- numeric(30)

for (i in 1:30){
  valid_kknn_ce <- kknn(V65 ~ .,
                           train = a1_train,
                           test = a1_valid,
                           k = i,
                           kernel = "rectangular"
  )

  a1_prob <- valid_kknn_ce$prob

  #column number for the true number in each row
  true_col <- match(as.character(a1_valid$V65), colnames(a1_prob))

  #Probability the model gave the true number, one value per image
  p_true <- a1_prob[cbind(1:nrow(a1_valid), true_col)]

  #cross-entropy, mean over validation images
  a1_ce_valid[i] <- -mean(log(p_true + 1e-15))
}

# Plot validation cross-entropy against K
a1_plot_ce <- function(){
  par(mar = c(5, 5, 2, 2))
  plot(1:30, a1_ce_valid, type = "b", pch = 19, col = "red",
       xlab = "K", ylab = "Cross-entropy (validation)")
}

a1_k_opt_ce <- which.min(a1_ce_valid)
a1_k_opt_ce

# optimal value: K = 6
