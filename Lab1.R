# Load necessary libraries
library(readr)    # For reading data
library(caret)    # For data partitioning and model evaluation
library(dplyr)    # For data manipulation

# Read the dataset
data <- read_csv("C:/Users/sande/Downloads/lab1/oulad-students.csv")

# Data preprocessing: Convert categorical variables to factors
data <- mutate_at(data, vars(code_module, code_presentation, gender, region, highest_education, imd_band, age_band, num_of_prev_attempts, disability, final_result), as.factor)

# Drop rows with missing values
data <- na.omit(data)

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(123) # For reproducibility
train_index <- createDataPartition(data$final_result, p = 0.8, list = FALSE) 
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the classification model (logistic regression)
model <- glm(final_result ~ ., data = train_data, family = binomial)

# Make predictions on the test data
probabilities <- predict(model, newdata = test_data, type = "response")
predictions <- ifelse(probabilities > 0.5, "Distinction", "Fail")

# Evaluate the model
confusion_matrix <- table(predictions, test_data$final_result)
confusion_matrix
