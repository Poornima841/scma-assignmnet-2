# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(e1071)
# Load the IPL dataset
ipl_data <- read.csv("path_to_your_dataset.csv")
# Check the structure of the data
str(ipl_data)

# Handle missing values
ipl_data <- na.omit(ipl_data)

# Convert necessary columns to factors (if any)
ipl_data$ColumnName <- as.factor(ipl_data$ColumnName)
# Visualize the distribution of a variable
ggplot(ipl_data, aes(x = ColumnName)) +
  geom_histogram(binwidth = 10) +
  theme_minimal()

# Pair plot to see relationships between variables
pairs(ipl_data)
# Split the data into training and testing sets
set.seed(123)
training_samples <- ipl_data$ColumnName %>%
  createDataPartition(p = 0.8, list = FALSE)
train_data  <- ipl_data[training_samples, ]
test_data <- ipl_data[-training_samples, ]
# Linear Regression
linear_model <- lm(Target ~ ., data = train_data)
summary(linear_model)

# Predict and evaluate
predictions <- predict(linear_model, test_data)
RMSE(predictions, test_data$Target)
R2(predictions, test_data$Target)

# Random Forest Regression
rf_model <- randomForest(Target ~ ., data = train_data)
predictions_rf <- predict(rf_model, test_data)
RMSE(predictions_rf, test_data$Target)
R2(predictions_rf, test_data$Target)

# SVM Regression
svm_model <- svm(Target ~ ., data = train_data)
predictions_svm <- predict(svm_model, test_data)
RMSE(predictions_svm, test_data$Target)
R2(predictions_svm, test_data$Target)
# Plot actual vs predicted
ggplot(test_data, aes(x = Target, y = predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = 'blue') +
  theme_minimal() +
  labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted")
