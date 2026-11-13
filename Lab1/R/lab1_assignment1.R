library(kknn)

# 1. + 2.
a1.data <- read.csv(r"(C:\732A99 MLFC\labs\lab1\optdigits.csv)", header = FALSE)
a1.data$V65 <- as.factor(a1.data$V65)
a1.n = dim(a1.data)[1]
set.seed(12345)
a1.id = sample(1:n, floor(n*0.5))
a1.train = a1.data[id, ]

a1.id1 = setdiff(1:n, a1.id)
a1.set.seed(12345)
a1.id2 = sample(a1.id2, floor(n*0.25))
a1.valid = a1.data[a1.id2, ]

a1.id3 = setdiff(a1.id1, a1.id2)
a1.test = a1.data[a1.id3, ]

a1.test.kknn <- kknn(
  V65 ~ .,
  train = a1.train,
  test = a1.test,
  k = 30,
  kernel = "rectangular"
)

a1.train.kknn <- kknn(V65 ~ .,
                   train = a1.train,
                   test = a1.train,
                   k = 30,
                   kernel = "rectangular"
                   )

a1.cm.test <- table(true = a1.test$V65, predicted = fitted(a1.test.kknn))
a1.n.errors.test <- nrow(a1.test) - sum(diag(a1.cm.test))
a1.misclass.rate.test <- a1.n.errors.test/nrow(a1.test)

a1.cm.train <- table(true = a1.train$V65, predicted = fitted(a1.train.kknn))
a1.n.errors.train <- nrow(a1.train) - sum(diag(a1.cm.train))
a1.misclass.rate.train <- a1.n.errors.train/nrow(a1.train)

#Comment: The 30-NN classifier has misclassification rates of 4.5% (training)
#and 5.3% (test), so about 95% of the digits are classified correctly. That
#is good quality overall, and the small gap between the two errors suggests
#little overfitting.

#Digits 0, 2, 6, 7 are classified almost perfectly. The hardest digits are
#4, 5, 8. Common confusions are 4 -> 7/9, 8 -> 1 and 5 -> 9, pairs that can
#look alike in the low-resolution 8x8 images.

# 3.

# Row # in train for all real 8's
a1.index8 <- which(a1.train$V65 == "8")

# Probability for the class "8" for just those rows
a1.prob8 <- a1.train.kknn$prob[a1.index8, "8"]

# Sort from lowest to highest probability and translate to rownumber in train
a1.index8.sorted <- a1.index8[order(a1.prob8)]

a1.hardest <- head(a1.index8.sorted, 3) # 3 lowest probabilities
a1.easiest <- tail(a1.index8.sorted, 2) # 2 highest probabilities

# Make row in train to 8x8-matrix and plot it
a1.plot_digit <- function(row, title){
  m <- matrix(as.numeric(train[row, 1:64]), nrow = 8, ncol = 8, byrow = TRUE)
  m <- m[8:1, ] # turn rows, heatmap plots first row on the bottom
  heatmap(m, Colv = NA, Rowv = NA, main = title)
}

for (r in a1.easiest) plot_digit(r, paste("Easy, p =", round(a1.train.kknn$prob[r, "8"], 2)))
for (r in a1.hardest) plot_digit(r, paste("Hard, p =", round(a1.train.kknn$prob[r, "8"], 2)))

# The easy ones are easy to tell they are 8's and the hard ones are difficult.
# 2 of the hard ones seem more likely to me to be ones and one of them could
# just as well be a six

# 4.

a1.misclass.train <- numeric(30)
a1.misclass.valid <- numeric(30)

for (i in 1:30){
  train.kknn.inner <- kknn(V65 ~ .,
                     train = a1.train,
                     test = a1.train,
                     k = i,
                     kernel = "rectangular"
  )
  valid.kknn.inner <- kknn(V65 ~ .,
                     train = a1.train,
                     test = a1.valid,
                     k = i,
                     kernel = "rectangular"
  )

  cm.train.inner <- table(true = a1.train$V65, predicted = fitted(train.kknn.inner))
  n.errors.train.inner <- nrow(a1.train) - sum(diag(cm.train.inner))
  misclass.rate.train.inner <- n.errors.train.inner/nrow(a1.train)

  cm.valid.inner <- table(true = a1.valid$V65, predicted = fitted(valid.kknn.inner))
  n.errors.valid.inner <- nrow(a1.valid) - sum(diag(cm.valid.inner))
  misclass.rate.valid.inner <- n.errors.valid.inner/nrow(a1.valid)

  a1.misclass.train[i] <- misclass.rate.train.inner
  a1.misclass.valid[i] <- misclass.rate.valid.inner
}

plot(1:30, a1.misclass.train, type = "b", col = "blue", pch = 19,
     ylim = range(c(a1.misclass.train, a1.misclass.valid)),
     xlab = "K", ylab = "Misclassification error")
lines(1:30, a1.misclass.valid, type = "b", col = "red", pch = 19)
legend("bottomright", legend = c("Training", "Validation"),
       col = c("blue", "red"), pch = 19, lty = 1)


# Optimmal K = lowest validation error
a1.k.opt <- which.min(misclass.valid)
a1.k.opt

# test error for optimal K
a1.test.kknn.opt <- kknn(V65 ~ .,
                      train = a1.train,
                      test = a1.test,
                      k = a1.k.opt,
                      kernel = "rectangular")

a1.cm.test.opt <- table(true = a1.test$V65, predicted = fitted(a1.test.kknn.opt))
a1.misclass.test.opt <- (nrow(a1.test) - sum(diag(a1.cm.test.opt))) / nrow(a1.test)

# Compare the three errors for optimal K
c(train = a1.misclass.train[a1.k.opt],
  valid = a1.misclass.valid[a1.k.opt],
  test = a1.misclass.test.opt)


# Higher K, lower complexity. The model learns the training data and can get low
# Errors, but is worse with generalization.
# Lower K, higher complexity, the model can generalize better.

#k.opt = 3.
# train      valid       test
# 0.01151230 0.02513089 0.02403344
#
# according to the plot, the optimal k is 3, where the validation error is lowest.
# For this K the misclassification errors are 1.2% for training, 2.5% for validation
# and 2.4% for test. As expected, the training error is lowest because the model
# has seen that data. The test error is very close to the validation error, so the
# validation error is a reliable estimate of performance on new data and the model
# generalizes well. With about 97.6% of the test digits classified correctly, the
# model quality is good.


# 5.
a1.ce.valid <- numeric(30)

for (i in 1:30){
  valid.kknn.ce <- kknn(V65 ~ .,
                           train = a1.train,
                           test = a1.valid,
                           k = i,
                           kernel = "rectangular"
  )

  a1.prob <- valid.kknn.ce$prob

  #column number for the true number in each row
  true.col <- match(as.character(a1.valid$V65), colnames(a1.prob))

  #Probability the model gave the true number, one bit per image
  p.true <- a1.prob[cbind(1:nrow(a1.valid), true.col)]

  #cross-entropy, mean over validation images
  a1.ce.valid[i] <- -mean(log(p.true + 1e-15))
}

par(mar = c(5,5,2,2))
plot(1:30, a1.ce.valid, type = "b", pch = 19, col = "red",
     xlab = "K", ylab = "Cross-entropy (validation)")

a1.k.opt.ce <- which.min(ce.valid)
a1.k.opt.ce


# optimal value: K = 6
# assuming that response has multinomial distribution, why might the cross entropy
# be a more suitable choice of the error-function than the misclassification error for this problem?

# misclassification error only shows how many misclassifications there are,
# leaving information such as how large are the misclassifications.
# for example, misclassification would give the same result if the model
# misclassified one instance with 1% or 90%, which isnt great because that
# misses information about the model's performance.






