#PART A. LOAD AND PREPARE THE DATA
## -----------------------------------------------------------------------------
install.packages(c(
  "dplyr",
  "caret",
  "e1071",
  "class",
  "purrr",
  "ggplot2",
  "pROC"
))

library(dplyr)
library(caret)
library(e1071)
library(class)
library(purrr)
library(ggplot2)
library(pROC)

# 1. Read directly from the raw data stream
PimaIndiansDiabetes <- read.csv(
  "https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv", 
  header = FALSE
)

# 2. Assign standard mlbench column headers
colnames(PimaIndiansDiabetes) <- c("pregnant", "glucose", "pressure", "triceps", 
                                   "insulin", "mass", "pedigree", "age", "diabetes")

# 3. Factor the target class to match ML standards
PimaIndiansDiabetes$diabetes <- factor(PimaIndiansDiabetes$diabetes, 
                                       levels = c(0, 1), 
                                       labels = c("neg", "pos"))

# data(PimaIndiansDiabetes)
Pima <- PimaIndiansDiabetes
glimpse(Pima)
# df$diabetes is our LABEL: "pos" (has diabetes) or "neg" (does not)
table(Pima$diabetes)   # check class balance -- links to Day 5 "Class Imbalance"

# Train / test split (Day 2 concept)
set.seed(42)
train_idx <- createDataPartition(Pima$diabetes, p = 0.75, list = FALSE)
train <- Pima[train_idx, ]
test  <- Pima[-train_idx, ]


## -----------------------------------------------------------------------------
## PART B. BAYESIAN DECISION THEORY -- THE INTUITION
## -----------------------------------------------------------------------------
# Bayes' rule (Day 1 & Day 3):
#     P(class | features) = P(features | class) * P(class) / P(features)
# In plain English: "Update your belief about the diagnosis, using the
# evidence (symptoms), starting from how common the disease is (the prior)."

# The class PRIORS are simply how common each outcome is in the data:
prop.table(table(train$diabetes))


## -----------------------------------------------------------------------------
## PART C. NAIVE BAYES CLASSIFIER (a direct application of Bayes' rule)
## -----------------------------------------------------------------------------
# "Naive" = it assumes every feature is independent of the others, given the
# class. This is rarely exactly true, but works surprisingly well in practice.
nb_model <- naiveBayes(diabetes ~ ., data = train)
nb_model                      # shows the learned P(feature | class) for each variable

nb_pred <- predict(nb_model, newdata = test)

# CONFUSION MATRIX (Day 2 concept, now applied)
confusionMatrix(nb_pred, test$diabetes, positive = "pos")

# MAP (Maximum A Posteriori) note: the predicted class above IS the MAP
# estimate -- we pick whichever class has the HIGHEST posterior probability.
nb_probs <- predict(nb_model, newdata = test, type = "raw")
head(nb_probs)   # columns = P(neg | x) and P(pos | x) for each patient


## -----------------------------------------------------------------------------
## PART D. k-NEAREST NEIGHBOURS (k-NN)
## -----------------------------------------------------------------------------
# k-NN has NO training phase -- it simply looks at the "k" most similar
# patients in the training set and takes a majority vote.
# IMPORTANT: distance-based methods need SCALED features (Day 4 concept:
# "distance metrics" are meaningless if variables are on very different scales).

num_cols <- setdiff(names(df), "diabetes")
train_scaled <- scale(train[, num_cols])
test_scaled  <- scale(test[, num_cols],
                       center = attr(train_scaled, "scaled:center"),
                       scale  = attr(train_scaled, "scaled:scale"))

knn_pred <- knn(train = train_scaled, test = test_scaled,
                cl = train$diabetes, k = 5)

confusionMatrix(knn_pred, test$diabetes, positive = "pos")
#............................................................
num_cols <- setdiff(names(df), "diabetes")
train_unscaled <- as.data.frame(train_unscaled)
test_unscaled <- as.data.frame(test_unscaled)

# View results
head(train_unscaled)
head(test_unscaled)

# Unscale test data
test_unscaled <- sweep(test_scaled, 2, train_center, FUN = "*")
test_unscaled <- sweep(test_unscaled, 2, train_scale, FUN = "+")

# Convert to data frames
train_unscaled <- as.data.frame(train_unscaled)
test_unscaled  <- as.data.frame(test_unscaled)
# HOW TO CHOOSE k?  Try several values and compare accuracy (a mini
# cross-validation, Day 2 concept):
k_values <- c(1, 3, 5, 7, 9, 11, 15)
accuracies <- map_dbl(k_values, function(k) {
  pred_k <- knn(train_scaled, test_scaled, cl = train$diabetes, k = k)
  mean(pred_k == test$diabetes)
})
tibble(k = k_values, accuracy = accuracies) %>%
  ggplot(aes(k, accuracy)) +
  geom_line(colour = "#2E86AB") + geom_point(size = 3, colour = "#F26430") +
  labs(title = "Choosing k for k-NN", x = "k (number of neighbours)",
       y = "Test Accuracy") + theme_minimal()


## -----------------------------------------------------------------------------
## PART E. SUPPORT VECTOR MACHINE (SVM)
## -----------------------------------------------------------------------------
# SVM finds the boundary that leaves the WIDEST possible margin between
# the two classes. The "support vectors" are the closest points to that
# boundary -- they are the only points that matter for the final model.

## --- E.1 Linear kernel (a straight-line boundary) ---------------------------
svm_linear <- svm(diabetes ~ ., data = train, kernel = "linear",
                   cost = 1, probability = TRUE)
svm_linear_pred <- predict(svm_linear, newdata = test)
confusionMatrix(svm_linear_pred, test$diabetes, positive = "pos")

## --- E.2 Radial (RBF/Gaussian) kernel: the "kernel trick" -------------------
# This lets SVM draw CURVED boundaries by implicitly mapping data into a
# higher-dimensional space -- without ever computing that mapping explicitly.
svm_radial <- svm(diabetes ~ ., data = train, kernel = "radial",
                   cost = 1, gamma = 0.1, probability = TRUE)
svm_radial_pred <- predict(svm_radial, newdata = test)
confusionMatrix(svm_radial_pred, test$diabetes, positive = "pos")

## --- E.3 Tuning cost (C) and gamma with cross-validation --------------------
tuned <- tune(svm, diabetes ~ ., data = train, kernel = "radial",
              ranges = list(cost = c(0.1, 1, 10), gamma = c(0.01, 0.1, 1)))
summary(tuned)
best_svm <- tuned$best.model


## -----------------------------------------------------------------------------
## PART F. COMPARE ALL THREE MODELS WITH ROC / AUC (Day 2 concept)
## -----------------------------------------------------------------------------
nb_prob_pos  <- nb_probs[, "pos"]
svm_prob     <- attr(predict(svm_radial, test, probability = TRUE), "probabilities")[, "pos"]

roc_nb  <- roc(test$diabetes, nb_prob_pos,  levels = c("neg", "pos"))
roc_svm <- roc(test$diabetes, svm_prob,     levels = c("neg", "pos"))

plot(roc_nb,  col = "#2E86AB", main = "ROC Curves: Naive Bayes vs SVM (radial)")
plot(roc_svm, col = "#F26430", add = TRUE)
legend("bottomright", legend = c(paste("Naive Bayes  AUC =", round(auc(roc_nb), 3)),
                                  paste("SVM (radial) AUC =", round(auc(roc_svm), 3))),
       col = c("#2E86AB", "#F26430"), lwd = 2)

## -----------------------------------------------------------------------------
## EXERCISE FOR TRAINEES
## -----------------------------------------------------------------------------
# 1. Which model had the highest AUC? Does that match what you'd expect?
# 2. Re-run k-NN with the UNSCALED data. Does performance get worse? Why?
# 3. Change the SVM cost parameter to 100. What happens to the number of
#    support vectors (svm_radial$tot.nSV)? Relate this to overfitting (Day 2).

