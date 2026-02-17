# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)
library(pastecs)  # For detailed descriptive statistics

# Read the processed data
f1_data <- read.csv("F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//f1_data_processed.csv",
                    stringsAsFactors = TRUE)
summary(f1_data)
# Calculate descriptive statistics for points by constructor
constructor_points_stats <- f1_data %>%
  group_by(constructor) %>%
  summarize(
    races = n(),
    total_points = sum(points, na.rm = TRUE),
    mean_points = mean(points, na.rm = TRUE),
    median_points = median(points, na.rm = TRUE),
    sd_points = sd(points, na.rm = TRUE),
    min_points = min(points, na.rm = TRUE),
    max_points = max(points, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_points))

# Print the top constructors by average points
print(head(constructor_points_stats, 10))

# For more detailed statistics on a specific constructor
ferrari_stats <- f1_data %>%
  filter(constructor == "Ferrari") %>%
  select(points)

stat.desc(ferrari_stats)

# Distribution of grid vs. finishing positions
position_change_stats <- f1_data %>%
  filter(!is.na(grid) & !is.na(position)) %>%
  mutate(position_change = grid - position) %>%
  group_by(constructor) %>%
  summarize(
    races = n(),
    avg_grid = mean(grid, na.rm = TRUE),
    avg_finish = mean(position, na.rm = TRUE),
    avg_position_change = mean(position_change, na.rm = TRUE),
    positive_changes = sum(position_change > 0, na.rm = TRUE),
    negative_changes = sum(position_change < 0, na.rm = TRUE),
    no_changes = sum(position_change == 0, na.rm = TRUE),
    pct_improved = positive_changes / races * 100
  ) %>%
  arrange(desc(avg_position_change))

print(head(position_change_stats, 10))

# Frequency of position improvements by driver
driver_improvements <- f1_data %>%
  filter(!is.na(grid) & !is.na(position)) %>%
  mutate(position_change = grid - position) %>%
  group_by(driver) %>%
  summarize(
    races = n(),
    improvements = sum(position_change > 0, na.rm = TRUE),
    improvement_rate = improvements / races,
    avg_positions_gained = mean(ifelse(position_change > 0, position_change, 0), na.rm = TRUE)
  ) %>%
  filter(races >= 10) %>%  # Filter drivers with at least 10 races
  arrange(desc(improvement_rate))

print(head(driver_improvements, 10))

# Frequency of position improvements by team
team_improvements <- f1_data %>%
  filter(!is.na(grid) & !is.na(position)) %>%
  mutate(position_change = grid - position) %>%
  group_by(constructor) %>%
  summarize(
    races = n(),
    improvements = sum(position_change > 0, na.rm = TRUE),
    improvement_rate = improvements / races,
    avg_positions_gained = mean(ifelse(position_change > 0, position_change, 0), na.rm = TRUE)
  ) %>%
  filter(races >= 10) %>%  # Filter teams with at least 10 races
  arrange(desc(improvement_rate))

print(head(team_improvements, 10))


# Create a scatterplot of qualifying position vs. points earned
qual_points_plot <- ggplot(f1_data, aes(x = qualifying_position, y = points)) +
  geom_point(aes(color = constructor), alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_continuous(breaks = seq(1, 24, by = 2)) +
  labs(title = "Relationship Between Qualifying Position and Points Earned",
       x = "Qualifying Position",
       y = "Points",
       color = "Constructor") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"))

print(qual_points_plot)

# Faceted by year to see trends over time
qual_points_by_year <- ggplot(f1_data, aes(x = qualifying_position, y = points)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  facet_wrap(~ year, scales = "free") +
  labs(title = "Qualifying Position vs. Points by Year",
       x = "Qualifying Position",
       y = "Points") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightblue"),
        strip.text = element_text(face = "bold"))

print(qual_points_by_year)

# Box plots of points by constructor
# Filter to top constructors for readability
top_constructors <- constructor_points_stats %>%
  filter(races >= 20) %>%  # Only constructors with sufficient races
  top_n(10, mean_points) %>%
  pull(constructor)

points_boxplot <- f1_data %>%
  filter(constructor %in% top_constructors) %>%
  ggplot(aes(x = reorder(constructor, points, FUN = median), y = points, fill = constructor)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Distribution of Points by Top Constructors",
       x = "Constructor",
       y = "Points") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

print(points_boxplot)

# Box plots with points overlay
points_boxplot_with_jitter <- f1_data %>%
  filter(constructor %in% top_constructors) %>%
  ggplot(aes(x = reorder(constructor, points, FUN = median), y = points, fill = constructor)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(title = "Distribution of Points by Top Constructors",
       x = "Constructor",
       y = "Points") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

print(points_boxplot_with_jitter)

# Line graphs showing performance trends across seasons for top constructors
constructor_yearly_performance <- f1_data %>%
  filter(constructor %in% top_constructors) %>%
  group_by(year, constructor) %>%
  summarize(
    races = n(),
    total_points = sum(points, na.rm = TRUE),
    avg_points_per_race = total_points / races,
    .groups = "drop"
  )

# Plot average points per race by year for top constructors
performance_trend_plot <- ggplot(constructor_yearly_performance, 
                                 aes(x = year, y = avg_points_per_race, color = constructor, group = constructor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Constructor Performance Trends Over Time",
       subtitle = "Average Points per Race by Year",
       x = "Year",
       y = "Average Points per Race",
       color = "Constructor") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"))

print(performance_trend_plot)

# Create a faceted version for clearer individual trends
performance_trend_facet <- ggplot(constructor_yearly_performance, 
                                  aes(x = year, y = avg_points_per_race, group = 1)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 2, color = "red") +
  facet_wrap(~ constructor, scales = "free_y") +
  labs(title = "Performance Trends by Constructor",
       x = "Year",
       y = "Average Points per Race") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5))

print(performance_trend_facet)

# Bar charts of position improvement by circuit type
circuit_type_improvements <- f1_data %>%
  filter(!is.na(grid) & !is.na(position)) %>%
  mutate(position_change = grid - position,
         improved = position_change > 0) %>%
  group_by(circuit_type) %>%
  summarize(
    races = n(),
    avg_position_change = mean(position_change, na.rm = TRUE),
    improvement_rate = mean(improved, na.rm = TRUE),
    avg_positions_gained = mean(ifelse(position_change > 0, position_change, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_position_change))

# Create bar chart for average position change by circuit type
position_change_by_circuit <- ggplot(circuit_type_improvements, 
                                     aes(x = reorder(circuit_type, avg_position_change), 
                                         y = avg_position_change,
                                         fill = avg_position_change)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  labs(title = "Average Position Change by Circuit Type",
       x = "Circuit Type",
       y = "Average Position Change (Positive = Improvement)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

print(position_change_by_circuit)

# Create bar chart for improvement rate by circuit type
improvement_rate_by_circuit <- ggplot(circuit_type_improvements, 
                                      aes(x = reorder(circuit_type, improvement_rate), 
                                          y = improvement_rate,
                                          fill = improvement_rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Position Improvement Rate by Circuit Type",
       subtitle = "Percentage of Races Where Drivers Finished Better Than They Qualified",
       x = "Circuit Type",
       y = "Improvement Rate") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

print(improvement_rate_by_circuit)

# Create a combined visualization showing improvement metrics by circuit type
circuit_type_improvements_long <- circuit_type_improvements %>%
  select(circuit_type, avg_position_change, avg_positions_gained, improvement_rate) %>%
  pivot_longer(cols = c(avg_position_change, avg_positions_gained, improvement_rate),
               names_to = "metric",
               values_to = "value")

# Create a faceted bar chart
circuit_improvements_faceted <- ggplot(circuit_type_improvements_long, 
                                       aes(x = reorder(circuit_type, value), 
                                           y = value,
                                           fill = circuit_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ metric, scales = "free_y", 
             labeller = labeller(metric = c(
               "avg_position_change" = "Average Position Change",
               "avg_positions_gained" = "Average Positions Gained",
               "improvement_rate" = "Improvement Rate"
             ))) +
  coord_flip() +
  labs(title = "Position Improvement Metrics by Circuit Type",
       x = "Circuit Type",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5))

print(circuit_improvements_faceted)

# Analyze position improvement by constructor and circuit type
constructor_circuit_improvements <- f1_data %>%
  filter(!is.na(grid) & !is.na(position) & constructor %in% top_constructors) %>%
  mutate(position_change = grid - position) %>%
  group_by(constructor, circuit_type) %>%
  summarize(
    races = n(),
    avg_position_change = mean(position_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(races >= 5)  # Only include combinations with sufficient data

# Create a heatmap of position changes by constructor and circuit type
constructor_circuit_heatmap <- ggplot(constructor_circuit_improvements, 
                                      aes(x = circuit_type, y = constructor, fill = avg_position_change)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0,
                       name = "Avg. Position\nChange") +
  labs(title = "Average Position Change by Constructor and Circuit Type",
       x = "Circuit Type",
       y = "Constructor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5))

print(constructor_circuit_heatmap)

# Create a combined dashboard of key visualizations
grid.arrange(
  qual_points_plot,
  points_boxplot,
  performance_trend_plot,
  position_change_by_circuit,
  ncol = 2
)

# Save the dashboard
ggsave("f1_analysis_dashboard.png", width = 16, height = 12, dpi = 300)
