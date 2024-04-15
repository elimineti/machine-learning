# Load necessary libraries
library(readr)    # For reading data
library(caret)    # For data partitioning and model evaluation
library(dplyr)    # For data manipulation
library(ggplot2)  # For visualization

# Read the datasets
students_data <- read_csv("C:/Users/srava/OneDrive/Desktop/oulad-students.csv")
assessments_data <- read_csv("C:/Users/srava/OneDrive/Desktop/oulad-assessments.csv")

# Data preprocessing: Convert categorical variables to factors
students_data <- mutate_at(students_data, vars(code_module, code_presentation, gender, region, highest_education, imd_band, age_band, num_of_prev_attempts, disability, final_result), as.factor)

# Drop rows with missing values from both datasets
students_data <- na.omit(students_data)
assessments_data <- na.omit(assessments_data)

# Merge datasets based on id_student, code_module, and code_presentation
merged_data <- merge(students_data, assessments_data, by = c("id_student", "code_module", "code_presentation"))

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(123) # For reproducibility
train_index <- createDataPartition(merged_data$final_result, p = 0.8, list = FALSE) 
train_data <- merged_data[train_index, ]
test_data <- merged_data[-train_index, ]

# Train the classification model (logistic regression)
classification_model <- glm(final_result ~ ., data = train_data, family = binomial)

# Train the regression model
regression_model <- lm(score ~ ., data = train_data)

# Make predictions on the test data for classification model
classification_probabilities <- predict(classification_model, newdata = test_data, type = "response")
classification_predictions <- ifelse(classification_probabilities > 0.5, "Distinction", "Fail")

# Make predictions on the test data for regression model
regression_predictions <- predict(regression_model, newdata = test_data)

# Evaluate the classification model
classification_confusion_matrix <- table(Predicted = classification_predictions, Actual = test_data$final_result)

# Visualize the confusion matrix for classification
ggplot(data = as.data.frame(as.table(classification_confusion_matrix)), aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Classification Confusion Matrix",
       x = "Predicted",
       y = "Actual",
       fill = "Frequency")

# Visualize the regression predictions
ggplot(data = test_data, aes(x = score, y = regression_predictions)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Regression Model Predictions",
       x = "Actual Score",
       y = "Predicted Score",
       color = "Predictions")
