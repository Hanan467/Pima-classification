# Load libraries
library(class)
library(MASS)
library(caret)

# Load data
data(Pima.tr)
data(Pima.te)

# Prepare data
train.x <- Pima.tr[, c("glu", "ped")]
train.y <- Pima.tr$type

# Function to compute 5-fold cross-validation error
get_cv_error <- function(k) {
  set.seed(123)  # Make results reproducible
  folds <- createFolds(train.y, k = 5)
  errors <- c()

  for (i in 1:5) {
    fold_idx <- folds[[i]]
    train_fold.x <- train.x[-fold_idx, ]
    train_fold.y <- train.y[-fold_idx]
    valid_fold.x <- train.x[fold_idx, ]
    valid_fold.y <- train.y[fold_idx]

    pred <- knn(train_fold.x, valid_fold.x, train_fold.y, k = k)
    error <- mean(pred != valid_fold.y)
    errors <- c(errors, error)
  }

  mean(errors)
}

# Run cross-validation for various K values
k_values <- c(1, 3, 5, 7, 9)
cv_errors <- sapply(k_values, get_cv_error)

# Output results
cv_results <- data.frame(K = k_values, CV_Error = cv_errors)
print(cv_results)

#Plot error vs K
plot(k_values, cv_errors, type = "b", col = "blue", pch = 19,
     xlab = "K", ylab = "Cross-Validation Error", main = "k-NN CV Error")
