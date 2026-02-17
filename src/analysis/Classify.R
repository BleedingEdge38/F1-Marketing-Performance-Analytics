# Load necessary libraries
library(tidyverse)
library(caret)
library(pROC)
library(sjPlot)

# Read the processed data
f1_data <- read.csv("F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//f1_data_processed.csv",
                    stringsAsFactors = TRUE)


# First, create the categorical outcome variable for podium finish
f1_data <- f1_data %>%
  mutate(podium_finish = ifelse(position <= 3, "Podium", "No Podium"),
         podium_finish = factor(podium_finish, levels = c("No Podium", "Podium")),
         points_finish = ifelse(points > 0, "Points", "No Points"),
         points_finish = factor(points_finish, levels = c("No Points", "Points")),
         marketing_exposure = case_when(
           position <= 3 ~ "High",
           position <= 10 ~ "Medium",
           TRUE ~ "Low"
         ),
         marketing_exposure = factor(marketing_exposure, 
                                     levels = c("Low", "Medium", "High")))

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(f1_data$podium_finish, p = 0.7, list = FALSE)
train_data <- f1_data[train_index, ]
test_data <- f1_data[-train_index, ]

# Build logistic regression model for podium finish
podium_model <- glm(podium_finish ~ grid + constructor_prestige + circuit_type, 
                    data = train_data, family = "binomial")

# Display model summary
summary_podium <- summary(podium_model)
print(summary_podium)

# Make predictions on test data
podium_probs <- predict(podium_model, newdata = test_data, type = "response")
podium_preds <- ifelse(podium_probs > 0.5, "Podium", "No Podium")
podium_preds <- factor(podium_preds, levels = c("No Podium", "Podium"))

# Create confusion matrix
conf_matrix <- confusionMatrix(podium_preds, test_data$podium_finish, positive = "Podium")
print(conf_matrix)

# Calculate additional metrics
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print metrics
cat("\nClassification Metrics:\n")
cat("Accuracy:", conf_matrix$overall["Accuracy"], "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Plot ROC curve
roc_obj <- roc(test_data$podium_finish, podium_probs)
auc_value <- auc(roc_obj)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve for Podium Finish Prediction",
     col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
text(0.7, 0.3, paste("AUC =", round(auc_value, 3)), col = "blue")

# Visualize the model coefficients
plot_model(podium_model, sort.est = TRUE, title = "Factors Predicting Podium Finish")

# Analyze the effect of grid position on podium probability
grid_effect <- data.frame(
  grid = 1:20,
  constructor_prestige = 2,  # Set to median value
  circuit_type = "Permanent Race Circuit"  # Most common circuit type
)

grid_effect$podium_prob <- predict(podium_model, newdata = grid_effect, type = "response")

ggplot(grid_effect, aes(x = grid, y = podium_prob)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Effect of Grid Position on Podium Probability",
       x = "Grid Position",
       y = "Probability of Podium Finish") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

# Analyze the effect of constructor prestige on podium probability
prestige_effect <- data.frame(
  grid = 5,  # Set to median value
  constructor_prestige = 1:6,
  circuit_type = "Permanent Race Circuit"  # Most common circuit type
)

prestige_effect$podium_prob <- predict(podium_model, newdata = prestige_effect, type = "response")

ggplot(prestige_effect, aes(x = constructor_prestige, y = podium_prob)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Effect of Constructor Prestige on Podium Probability",
       x = "Constructor Prestige (lower is more prestigious)",
       y = "Probability of Podium Finish") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:6)

# Analyze false positives and false negatives
misclassified <- test_data %>%
  mutate(predicted = podium_preds,
         actual = podium_finish,
         error_type = case_when(
           predicted == "Podium" & actual == "No Podium" ~ "False Positive",
           predicted == "No Podium" & actual == "Podium" ~ "False Negative",
           TRUE ~ "Correct"
         ))

# Analyze false positives (predicted podium but didn't achieve it)
false_positives <- misclassified %>%
  filter(error_type == "False Positive") %>%
  select(year, race_name, driver, constructor, grid, position, points, constructor_prestige, circuit_type)

# Analyze false negatives (didn't predict podium but achieved it)
false_negatives <- misclassified %>%
  filter(error_type == "False Negative") %>%
  select(year, race_name, driver, constructor, grid, position, points, constructor_prestige, circuit_type)

# Print summary of false positives and negatives
cat("\nFalse Positive Summary (Predicted Podium but didn't achieve it):\n")
print(summary(false_positives))

cat("\nFalse Negative Summary (Didn't predict Podium but achieved it):\n")
print(summary(false_negatives))

# Create a model for points finish as well
points_model <- glm(points_finish ~ grid + constructor_prestige + circuit_type, 
                    data = train_data, family = "binomial")

# Display model summary
summary_points <- summary(points_model)
print(summary_points)

# Make predictions for points finish
points_probs <- predict(points_model, newdata = test_data, type = "response")
points_preds <- ifelse(points_probs > 0.5, "Points", "No Points")
points_preds <- factor(points_preds, levels = c("No Points", "Points"))

# Create confusion matrix for points finish
points_conf_matrix <- confusionMatrix(points_preds, test_data$points_finish, positive = "Points")
print(points_conf_matrix)

# Create a model for marketing exposure
# Convert marketing_exposure to binary for simplicity (High vs. not High)
train_data$high_exposure <- ifelse(train_data$marketing_exposure == "High", "High", "Not High")
train_data$high_exposure <- factor(train_data$high_exposure, levels = c("Not High", "High"))

test_data$high_exposure <- ifelse(test_data$marketing_exposure == "High", "High", "Not High")
test_data$high_exposure <- factor(test_data$high_exposure, levels = c("Not High", "High"))

exposure_model <- glm(high_exposure ~ grid + constructor_prestige + circuit_type, 
                      data = train_data, family = "binomial")

# Display model summary
summary_exposure <- summary(exposure_model)
print(summary_exposure)

# Make predictions for marketing exposure
exposure_probs <- predict(exposure_model, newdata = test_data, type = "response")
exposure_preds <- ifelse(exposure_probs > 0.5, "High", "Not High")
exposure_preds <- factor(exposure_preds, levels = c("Not High", "High"))

# Create confusion matrix for marketing exposure
exposure_conf_matrix <- confusionMatrix(exposure_preds, test_data$high_exposure, positive = "High")
print(exposure_conf_matrix)

