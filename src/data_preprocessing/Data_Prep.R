# Load necessary library
library(dplyr)

# Read the CSV files
races <- read.csv("F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//races.csv") %>% 
  rename(race_name = name)
results <- read.csv("F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//results.csv") 
qualifying <- read.csv('F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//qualifying.csv')
drivers <- read.csv('F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//drivers.csv')
constructors <- read.csv('F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//constructors.csv') %>% 
  rename(constructor_name = name)
circuits <- read.csv('F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//circuits.csv') %>% 
  rename(circuit_name = name)
status <- read.csv('F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//status.csv')
constructor_standings <- read.csv('F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//constructor_standings.csv')

# Define constructor prestige tiers
modern_era_tiers <- c(
  "Mercedes" = 1,  # Dominated hybrid era
  "Red Bull" = 2,  # Strong 2010-2013, resurgent later
  "Ferrari" = 2,   # Consistent challenger
  "McLaren" = 3,   # Mixed results
  "Renault" = 4,
  "Williams" = 4,
  "Force India" = 5
)

# Perform the joins and filtering
final_data <- races %>%
  filter(year >= 2010 & year <= 2020) %>%
  inner_join(results, by = "raceId") %>%
  inner_join(drivers, by = "driverId") %>%
  inner_join(constructors, by = "constructorId") %>%
  inner_join(circuits, by = "circuitId") %>%
  inner_join(status, by = "statusId") %>%
  left_join(qualifying, by = c("raceId", "driverId")) %>%
  transmute(
    year = year,
    round = round,
    race_name = race_name,
    circuit = circuit_name,
    circuit_type = CircuitType,
    country = country,
    location = location,
    driver = surname,
    driver_nationality = nationality.x,
    constructor = constructor_name,
    constructor_nationality = nationality.y,
    grid = grid,
    position = position.x,
    points = points,
    status = status,
    q1 = q1,
    q2 = q2,
    q3 = q3,
    qualifying_position = position.y       # from qualifying
  )
# Add constructor prestige to the final dataset
final_data$constructor_prestige <- ifelse(
  final_data$constructor %in% names(modern_era_tiers),
  modern_era_tiers[final_data$constructor],
  6  # Default for backmarker teams
)

# View the final data
write.csv(final_data, "F://UoB Study//Marketing Analysis & Behaviour Science//Assignment 2//Data//final_data1.csv", row.names = FALSE)
