library(MASS)
library(e1071)

help(naiveBayes)

# Load the data
data(Pima.tr)
data(Pima.te)

# Explore the data
help(Pima.tr)
head(Pima.tr)
summary(Pima.tr)
dim(Pima.te)

# Train the Naive Bayes model using glu and ped as predictors
model_nb <- naiveBayes(type ~ glu + ped, data = Pima.tr)

# Show prior probabilities of each class (No and Yes)
model_nb$apriori

# Predict on the test dataset
predictions <- predict(model_nb, Pima.te)

# Confusion matrix
conf_mat <- table(Predicted = predictions, Actual = Pima.te$type)
print(conf_mat)

# Extract values from confusion matrix
TP <- conf_mat["Yes", "Yes"]
TN <- conf_mat["No", "No"]
FP <- conf_mat["Yes", "No"]
FN <- conf_mat["No", "Yes"]

# Compute evaluation metrics
accuracy <- (TP + TN) / sum(conf_mat)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

# Display the results
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Sensitivity:", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")
cat("F1 Score:", round(f1_score, 3), "\n")
