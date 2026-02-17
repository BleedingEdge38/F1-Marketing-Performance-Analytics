# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(effects)
library(interactions)
library(sjPlot)

# Read the processed data
f1_data <- read.csv("F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//f1_data_processed.csv",
                    stringsAsFactors = TRUE)


# Build the first regression model without interaction
model1 <- lm(points ~ grid + constructor_prestige, data = f1_data)
summary_model1 <- summary(model1)

# Build the second regression model with interaction
model2 <- lm(points ~ grid + circuit_type + grid*circuit_type, data = f1_data)
summary_model2 <- summary(model2)

anova(model1, model2)


# Display the model summary
print(summary_model2)

# Create a data frame for plotting predicted values by circuit type
grid_range <- 1:20
circuit_types <- unique(f1_data$circuit_type)
prediction_data <- expand.grid(grid = grid_range, 
                               circuit_type = circuit_types)

# Generate predictions
prediction_data$predicted_points <- predict(model2, newdata = prediction_data)

# Create visualization highlighting the interaction effect
ggplot(prediction_data, aes(x = grid, y = predicted_points, color = circuit_type)) +
  geom_line(size = 1) +
  labs(title = "Interaction Between Grid Position and Circuit Type",
       subtitle = "Effect on Expected Race Points",
       x = "Grid Position",
       y = "Predicted Points",
       color = "Circuit Type") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 20, by = 2)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Create a more detailed visualization with actual data points
ggplot(f1_data, aes(x = grid, y = points, color = circuit_type)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ circuit_type) +
  labs(title = "Relationship Between Grid Position and Points by Circuit Type",
       x = "Grid Position",
       y = "Points",
       color = "Circuit Type") +
  theme_minimal() +
  theme(legend.position = "none")

# Calculate average points by grid position and circuit type for a clearer visualization
grid_circuit_points <- f1_data %>%
  group_by(grid, circuit_type) %>%
  summarize(avg_points = mean(points, na.rm = TRUE),
            count = n(),
            .groups = "drop") %>%
  filter(count >= 5)  # Only include combinations with sufficient data

# Create a visualization of average points by grid position for each circuit type
ggplot(grid_circuit_points, aes(x = grid, y = avg_points, color = circuit_type)) +
  geom_point(aes(size = count), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Average Points by Grid Position and Circuit Type",
       x = "Grid Position",
       y = "Average Points",
       color = "Circuit Type",
       size = "Number of Races") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 20, by = 2)) +
  theme(legend.position = "right")

# Create an interaction plot for a clearer visualization of the effect
interact_plot(model2, pred = grid, modx = circuit_type, 
              plot.points = TRUE, point.alpha = 0.3,
              colors = "Set1",
              main.title = "Interaction Effect of Grid Position and Circuit Type on Points",
              x.label = "Grid Position",
              y.label = "Predicted Points",
              legend.main = "Circuit Type") +
  theme_minimal()

# Calculate the slopes (effect of grid position) for each circuit type
circuit_slopes <- data.frame(circuit_type = character(),
                             slope = numeric(),
                             stringsAsFactors = FALSE)

for (circuit in circuit_types) {
  circuit_data <- f1_data[f1_data$circuit_type == circuit, ]
  circuit_model <- lm(points ~ grid, data = circuit_data)
  circuit_slopes <- rbind(circuit_slopes, 
                          data.frame(circuit_type = circuit,
                                     slope = coef(circuit_model)[2],
                                     stringsAsFactors = FALSE))
}

# Sort by slope to see which circuit types have the strongest grid position effect
circuit_slopes <- circuit_slopes %>% arrange(slope)

# Create a bar chart of the slopes
ggplot(circuit_slopes, aes(x = reorder(circuit_type, slope), y = slope)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Effect of Grid Position on Points by Circuit Type",
       subtitle = "More negative values indicate stronger grid position advantage",
       x = "Circuit Type",
       y = "Effect of One Grid Position on Points") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Create a markdown interpretation of the results
cat("## Interpretation of Interaction Model Results\n\n")
cat(paste("R-squared:", round(summary_model2$r.squared, 3), "\n"))
cat(paste("Adjusted R-squared:", round(summary_model2$adj.r.squared, 3), "\n\n"))
cat("### Interaction Effect Interpretation:\n\n")
cat("The interaction terms in the model reveal how the effect of grid position on race points varies by circuit type:\n\n")

for (i in 1:length(circuit_slopes$circuit_type)) {
  circuit <- circuit_slopes$circuit_type[i]
  slope <- circuit_slopes$slope[i]
  cat(paste("- ", circuit, ": Each grid position improvement is worth approximately", 
            abs(round(slope, 2)), "points\n"))
}
