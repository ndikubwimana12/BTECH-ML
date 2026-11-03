#kNN CLASSIFICATION
# ============================================================

# Install once if necessary:
# install.packages("class")
library(tidyverse)
library(class)
library(ISLR2)

set.seed(12345)

# Split data: 70% training, 30% testing
n <- nrow(Default)
dim(Default)
train_id <- sample(
  1:n,
  size = 0.70 * n
)

train <- Default[train_id, ]
dim(train) # checking the data dimission
test <- Default[-train_id, ]


# Use numeric predictors for this beginner kNN example
x_train <- train[, c("balance", "income")]
x_test <- test[, c("balance", "income")]

y_train <- train$default
y_test <- test$default


# IMPORTANT: kNN uses distance, so scale the predictors
x_train_scaled <- scale(x_train)

x_test_scaled <- scale(
  x_test,
  center = attr(x_train_scaled, "scaled:center"),
  scale = attr(x_train_scaled, "scaled:scale")
)

# Interpretation:
# Scaling puts balance and income on comparable scales.
# The test data must use the training mean and standard deviation.


# kNN classification with k = 5 to avoid tie
knn_pred <- knn(
  train = x_train_scaled,
  test = x_test_scaled,
  cl = y_train,
  k = 5
)

# Confusion matrix for evaluation
table(
  Actual = y_test,
  Predicted = knn_pred
)

# Accuracy
mean(knn_pred == y_test)

# Interpretation:
# kNN classifies each test customer according to the classes
# of its nearest neighbours.


# Try several values of k
k_values <- c(1, 3,4, 5,6,7, 11, 15)

accuracy <- numeric(length(k_values))

for (i in 1:length(k_values)) {

  pred <- knn(
    train = x_train_scaled,
    test = x_test_scaled,
    cl = y_train,
    k = k_values[i]
  )

  accuracy[i] <- mean(pred == y_test)
}

data.frame(
  k = k_values,
  Accuracy = accuracy
)

# Interpretation:
# Compare the accuracy values.
# A higher test accuracy is better.


# ============================================================
# PART B: kNN REGRESSION
# ============================================================

data(mtcars)

head(mtcars)
str(mtcars)


# Split data: 75% training, 025% testing
set.seed(123)

n <- nrow(mtcars)

train_id <- sample(
  1:n,
  size = 0.75 * n
)

train <- mtcars[train_id, ]
test <- mtcars[-train_id, ]


# Response variable
y_train <- train$mpg
y_test <- test$mpg

# Predictor variables
x_train <- train[, c("wt", "hp", "disp", "cyl")]
x_test <- test[, c("wt", "hp", "disp", "cyl")]


# Scale predictors
x_train_scaled <- scale(x_train)

x_test_scaled <- scale(
  x_test,
  center = attr(x_train_scaled, "scaled:center"),
  scale = attr(x_train_scaled, "scaled:scale")
)

# Interpretation:
# Scaling is important because kNN calculates distances.


# Install once if necessary:
install.packages("FNN")

library(FNN)

# kNN regression with k = 5
knn_model <- knn.reg(
  train = x_train_scaled,
  test = x_test_scaled,
  y = y_train,
  k = 5
)

prediction <- knn_model$pred

data.frame(
  Actual = y_test,
  Predicted = prediction
)


# Calculate RMSE
rmse <- sqrt(
  mean((y_test - prediction)^2)
)
