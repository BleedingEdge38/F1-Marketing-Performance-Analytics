# Load required libraries
library(tidyverse)
library(naniar)  # For handling missing values
library(lubridate)  # For date manipulation

# Read the CSV file
f1_data <- read.csv("F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//final_data1.csv", stringsAsFactors = FALSE)

# Examine the structure of the data
str(f1_data)
summary(f1_data)

# Check for missing values
miss_var_summary(f1_data)

# Replace "\N" with NA for qualifying times
f1_data <- f1_data %>%
  mutate(across(c(q1, q2, q3), ~ifelse(. == "\\N", NA, .)))

# Convert qualifying times to seconds for easier analysis
convert_time_to_seconds <- function(time_str) {
  if (is.na(time_str)) return(NA)
  
  parts <- strsplit(time_str, ":")[[1]]
  if (length(parts) == 2) {
    minutes <- as.numeric(parts[1])
    seconds <- as.numeric(parts[2])
    return(minutes * 60 + seconds)
  } else {
    return(as.numeric(time_str))
  }
}

f1_data <- f1_data %>%
  mutate(
    q1_seconds = sapply(q1, convert_time_to_seconds),
    q2_seconds = sapply(q2, convert_time_to_seconds),
    q3_seconds = sapply(q3, convert_time_to_seconds)
  )

# For missing qualifying times, we can impute using the median of the same circuit/year
f1_data <- f1_data %>%
  group_by(year, circuit) %>%
  mutate(
    q1_seconds = ifelse(is.na(q1_seconds), median(q1_seconds, na.rm = TRUE), q1_seconds),
    q2_seconds = ifelse(is.na(q2_seconds), median(q2_seconds, na.rm = TRUE), q2_seconds),
    q3_seconds = ifelse(is.na(q3_seconds), median(q3_seconds, na.rm = TRUE), q3_seconds)
  ) %>%
  ungroup()

# Convert categorical variables to factors
f1_data <- f1_data %>%
  mutate(
    driver = as.factor(driver),
    driver_nationality = as.factor(driver_nationality),
    constructor = as.factor(constructor),
    constructor_nationality = as.factor(constructor_nationality),
    circuit = as.factor(circuit),
    circuit_type = as.factor(circuit_type),
    country = as.factor(country),
    location = as.factor(location),
    status = as.factor(status)
  )

# Create a binary variable for race completion
f1_data <- f1_data %>%
  mutate(race_completed = ifelse(status == "Finished" | grepl("\\+\\d+ Lap", status), 1, 0))

# Convert grid and position to numeric, handling special cases
f1_data <- f1_data %>%
  mutate(
    grid = as.numeric(ifelse(grid == "0", NA, grid)),
    position = as.numeric(ifelse(position == "\\N", NA, position))
  )

# Position change (start vs. finish)
f1_data <- f1_data %>%
  mutate(position_change = grid - position)

# Points per race average by driver and constructor
driver_points_avg <- f1_data %>%
  group_by(year, driver) %>%
  summarize(
    races = n(),
    total_points = sum(points, na.rm = TRUE),
    avg_points_per_race = total_points / races,
    .groups = "drop"
  )

constructor_points_avg <- f1_data %>%
  group_by(year, constructor) %>%
  summarize(
    races = n(),
    total_points = sum(points, na.rm = TRUE),
    avg_points_per_race = total_points / races,
    .groups = "drop"
  )

# Add these averages back to the main dataset
f1_data <- f1_data %>%
  left_join(driver_points_avg, by = c("year", "driver")) %>%
  left_join(constructor_points_avg, by = c("year", "constructor"), 
            suffix = c("", "_constructor"))

# Calculate qualifying performance (average position)
f1_data <- f1_data %>%
  group_by(year, driver) %>%
  mutate(
    avg_qualifying_position = mean(qualifying_position, na.rm = TRUE),
    qualifying_vs_avg = qualifying_position - avg_qualifying_position
  ) %>%
  ungroup()

# Calculate finish rate
driver_finish_rate <- f1_data %>%
  group_by(year, driver) %>%
  summarize(
    races = n(),
    finishes = sum(race_completed, na.rm = TRUE),
    finish_rate = finishes / races,
    .groups = "drop"
  )

f1_data <- f1_data %>%
  left_join(driver_finish_rate, by = c("year", "driver"), 
            suffix = c("", "_finish"))

# Create a performance index (weighted combination of points, position changes, and qualifying)
f1_data <- f1_data %>%
  mutate(
    performance_index = (points * 0.5) + 
      (position_change * 0.3) + 
      ((24 - qualifying_position) * 0.2)  # Assuming max grid size of 24
  )

# Fill missing positions with a value higher than the maximum grid size
max_grid_size <- max(f1_data$grid, na.rm = TRUE)

f1_data <- f1_data %>%
  mutate(position = ifelse(is.na(position), max_grid_size + 1, position))

# Create a DNF (Did Not Finish) indicator
f1_data <- f1_data %>%
  mutate(dnf = ifelse(status != "Finished" & !grepl("\\+\\d+ Lap", status), 1, 0))

# Save the processed data
write.csv(f1_data, "F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//f1_data_processed.csv", 
          row.names = FALSE)

# Create a summary dataset for analysis
f1_summary <- f1_data %>%
  group_by(year, driver, constructor) %>%
  summarize(
    races = n(),
    wins = sum(position == 1, na.rm = TRUE),
    podiums = sum(position <= 3, na.rm = TRUE),
    points = sum(points, na.rm = TRUE),
    avg_grid = mean(grid, na.rm = TRUE),
    avg_finish = mean(position, na.rm = TRUE),
    dnfs = sum(dnf),
    finish_rate = 1 - (dnfs / races),
    avg_performance_index = mean(performance_index, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(f1_summary, "F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//f1_summary.csv", 
          row.names = FALSE)
