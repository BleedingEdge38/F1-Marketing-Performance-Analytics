# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(effects)

# Read the processed data
f1_data <- read.csv("F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//f1_data_processed.csv",
                    stringsAsFactors = TRUE)


# Build the first regression model without interaction
model1 <- lm(points ~ grid + constructor_prestige, data = f1_data)
summary_model1 <- summary(model1)

# Display the model summary
print(summary_model1)

# Calculate predicted values
f1_data$predicted_points <- predict(model1, newdata = f1_data)

# Create visualization of model predictions vs. actual values
ggplot(f1_data, aes(x = predicted_points, y = points)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Model Predictions vs. Actual Points",
       x = "Predicted Points",
       y = "Actual Points") +
  theme_minimal() +
  annotate("text", x = max(f1_data$predicted_points) * 0.8, 
           y = max(f1_data$points) * 0.2, 
           label = paste("R² =", round(summary_model1$r.squared, 3)))

# Create a visualization showing the relationship between grid position and points
# with different colors for constructor prestige
ggplot(f1_data, aes(x = grid, y = points, color = as.factor(constructor_prestige))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between Grid Position and Points",
       x = "Grid Position",
       y = "Points",
       color = "Constructor Prestige") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# Create a coefficient plot for easier interpretation
coef_data <- data.frame(
  variable = c("Intercept", "Grid Position", "Constructor Prestige"),
  estimate = c(coef(model1)[1], coef(model1)[2], coef(model1)[3]),
  se = c(summary_model1$coefficients[1,2], 
         summary_model1$coefficients[2,2], 
         summary_model1$coefficients[3,2])
)

ggplot(coef_data, aes(x = variable, y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, 
                    ymax = estimate + 1.96 * se), 
                width = 0.2) +
  labs(title = "Regression Coefficients with 95% Confidence Intervals",
       x = "",
       y = "Coefficient Estimate") +
  theme_minimal() +
  coord_flip()

# Calculate average points by grid position for a more intuitive visualization
grid_points <- f1_data %>%
  group_by(grid) %>%
  summarize(avg_points = mean(points, na.rm = TRUE),
            count = n()) %>%
  filter(count >= 5)  # Only include grid positions with sufficient data

ggplot(grid_points, aes(x = grid, y = avg_points)) +
  geom_point(aes(size = count), alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red") +
  labs(title = "Average Points by Grid Position",
       x = "Grid Position",
       y = "Average Points",
       size = "Number of Races") +
  theme_minimal()

