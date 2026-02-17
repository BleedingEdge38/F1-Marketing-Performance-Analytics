# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)
library(corrplot)  # For correlation visualization
library(GGally)    # For advanced plot matrices

# Read the processed data
f1_data <- read.csv("F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//f1_data_processed.csv", stringsAsFactors = TRUE)

# Basic summary of key variables from the data dictionary
key_vars <- f1_data %>% 
  select(year, grid, position, points, qualifying_position, 
         constructor_prestige, position_change, races, 
         race_completed, dnf)

# Generate comprehensive summary statistics
summary_stats <- summary(key_vars)
print(summary_stats)

# More detailed statistics for numeric variables
numeric_stats <- key_vars %>%
  select(where(is.numeric)) %>%
  summarize(across(everything(), 
                   list(
                     mean = ~mean(., na.rm = TRUE),
                     median = ~median(., na.rm = TRUE),
                     sd = ~sd(., na.rm = TRUE),
                     min = ~min(., na.rm = TRUE),
                     max = ~max(., na.rm = TRUE),
                     q1 = ~quantile(., 0.25, na.rm = TRUE),
                     q3 = ~quantile(., 0.75, na.rm = TRUE)
                   )))

# Reshape for better presentation
numeric_stats_long <- numeric_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_pattern = "(.*)_(.*)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

print(numeric_stats_long)

# Categorical variable analysis
categorical_stats <- f1_data %>%
  summarize(
    circuit_types = n_distinct(circuit_type),
    constructors = n_distinct(constructor),
    drivers = n_distinct(driver),
    race_completion_rate = mean(race_completed, na.rm = TRUE),
    dnf_rate = mean(dnf, na.rm = TRUE)
  )

print(categorical_stats)

# Constructor prestige analysis
prestige_stats <- f1_data %>%
  group_by(constructor_prestige) %>%
  summarize(
    count = n(),
    avg_points = mean(points, na.rm = TRUE),
    avg_grid = mean(grid, na.rm = TRUE),
    avg_finish = mean(position, na.rm = TRUE),
    avg_position_change = mean(position_change, na.rm = TRUE),
    finish_rate = mean(race_completed, na.rm = TRUE)
  )

print(prestige_stats)

# Enhanced descriptive statistics by constructor
constructor_stats <- f1_data %>%
  group_by(constructor) %>%
  summarize(
    races = n(),
    total_points = sum(points, na.rm = TRUE),
    mean_points = mean(points, na.rm = TRUE),
    median_points = median(points, na.rm = TRUE),
    avg_grid = mean(grid, na.rm = TRUE),
    avg_finish = mean(position, na.rm = TRUE),
    avg_position_change = mean(grid - position, na.rm = TRUE),
    finish_rate = mean(race_completed, na.rm = TRUE),
    dnf_rate = mean(dnf, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_points))

# Print top constructors
print(head(constructor_stats, 10))

# Analyze performance by circuit type
circuit_performance <- f1_data %>%
  group_by(circuit_type) %>%
  summarize(
    races = n(),
    avg_points = mean(points, na.rm = TRUE),
    avg_position_change = mean(grid - position, na.rm = TRUE),
    finish_rate = mean(race_completed, na.rm = TRUE),
    dnf_rate = mean(dnf, na.rm = TRUE)
  )

# Visualize circuit type performance
ggplot(circuit_performance, aes(x = reorder(circuit_type, avg_points), y = avg_points, fill = avg_points)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Points by Circuit Type",
       x = "Circuit Type", y = "Average Points") +
  theme_minimal()


# Performance trends over years
yearly_trends <- f1_data %>%
  group_by(year) %>%
  summarize(
    races = n(),
    avg_points = mean(points, na.rm = TRUE),
    avg_position_change = mean(grid - position, na.rm = TRUE),
    finish_rate = mean(race_completed, na.rm = TRUE),
    dnf_rate = mean(dnf, na.rm = TRUE)
  )

# Visualize yearly trends
ggplot(yearly_trends, aes(x = year)) +
  geom_line(aes(y = avg_position_change, color = "Position Change"), size = 1) +
  geom_line(aes(y = finish_rate * 10, color = "Finish Rate (scaled)"), size = 1) +
  geom_line(aes(y = dnf_rate * 10, color = "DNF Rate (scaled)"), size = 1) +
  scale_y_continuous(
    name = "Position Change",
    sec.axis = sec_axis(~./10, name = "Rate (0-1)")
  ) +
  labs(title = "F1 Performance Trends Over Time", x = "Year", color = "Metric") +
  theme_minimal()


# Create a heatmap of grid vs. finishing positions
position_heatmap <- f1_data %>%
  filter(!is.na(grid) & !is.na(position) & grid <= 20 & position <= 20) %>%
  count(grid, position) %>%
  ggplot(aes(x = grid, y = position, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Heatmap of Grid vs. Finishing Positions",
       x = "Grid Position", y = "Finishing Position",
       fill = "Count") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 20, by = 1)) +
  scale_y_continuous(breaks = seq(1, 20, by = 1))

# Analyze performance by constructor prestige
prestige_performance <- f1_data %>%
  group_by(constructor_prestige) %>%
  summarize(
    count = n(),
    avg_points = mean(points, na.rm = TRUE),
    avg_grid = mean(grid, na.rm = TRUE),
    avg_finish = mean(position, na.rm = TRUE),
    avg_position_change = mean(grid - position, na.rm = TRUE),
    finish_rate = mean(race_completed, na.rm = TRUE)
  )

# Visualize prestige impact
ggplot(prestige_performance, aes(x = as.factor(constructor_prestige), y = avg_points, fill = as.factor(constructor_prestige))) +
  geom_bar(stat = "identity") +
  labs(title = "Average Points by Constructor Prestige",
       x = "Constructor Prestige (1 = Highest)", y = "Average Points") +
  theme_minimal() +
  theme(legend.position = "none")

# Select numeric variables for correlation analysis
numeric_vars <- f1_data %>%
  select(grid, position, points, qualifying_position, constructor_prestige, 
         position_change, races, dnf, finish_rate)

# Create correlation matrix
corr_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Visualize correlations
corrplot(corr_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Between F1 Performance Metrics",
mar = c(0, 0, 2, 0))

