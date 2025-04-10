#wild fire propagation model



# Wildfire Propagation Model in R
# This model simulates wildfire spread accounting for vegetation and climate factors

library(ggplot2)
library(reshape2)
library(dplyr)

# Set random seed for reproducibility
set.seed(123)

# Grid parameters
grid_size <- 50
num_steps <- 40

# Initialize grid: 0 = no vegetation, 1 = unburned vegetation, 2 = burning, 3 = burned
initialize_grid <- function(vegetation_density) {
  # Create grid with vegetation based on density parameter
  grid <- matrix(0, nrow = grid_size, ncol = grid_size)
  for (i in 1:grid_size) {
    for (j in 1:grid_size) {
      if (runif(1) < vegetation_density) {
        grid[i, j] <- 1  # Unburned vegetation
      }
    }
  }
  
  # Start fire in the center
  center <- ceiling(grid_size/2)
  grid[center, center] <- 2  # Burning
  
  return(grid)
}

# Compute fire spread probability based on climate and vegetation
calc_spread_probability <- function(temperature, wind_speed, wind_direction, humidity, veg_type, 
                                    from_cell_i, from_cell_j, to_cell_i, to_cell_j) {
  
  # Base probability
  base_prob <- 0.3
  
  # Temperature effect (higher temperature increases spread)
  temp_factor <- min(2.0, max(0.5, temperature / 25))
  
  # Humidity effect (lower humidity increases spread)
  humidity_factor <- max(0.3, 1 - (humidity / 100))
  
  # Vegetation type effect
  veg_factor <- switch(veg_type,
                       "grass" = 1.5,     # Burns quickly
                       "brush" = 1.2,     # Burns moderately
                       "forest" = 0.9,    # Burns more slowly but intensely
                       "mixed" = 1.0)     # Average burn rate
  
  # Wind effect
  # Calculate direction from burning cell to potential new cell
  delta_i <- to_cell_i - from_cell_i
  delta_j <- to_cell_j - from_cell_j
  
  # Convert wind direction from degrees to radians (0° = North, 90° = East)
  wind_rad <- wind_direction * pi / 180
  
  # Wind vector components
  wind_i <- -cos(wind_rad)  # Negative because lower i is "north"
  wind_j <- sin(wind_rad)
  
  # Dot product to determine if wind is pushing fire in this direction
  # Normalize cell direction vector
  length <- sqrt(delta_i^2 + delta_j^2)
  if (length > 0) {
    norm_delta_i <- delta_i / length
    norm_delta_j <- delta_j / length
    
    wind_alignment <- norm_delta_i * wind_i + norm_delta_j * wind_j
    wind_factor <- 1 + (wind_alignment * wind_speed / 20)
    wind_factor <- max(0.5, min(2.5, wind_factor))  # Limit the effect
  } else {
    wind_factor <- 1.0
  }
  
  # Calculate final probability
  final_prob <- base_prob * temp_factor * humidity_factor * veg_factor * wind_factor
  
  # Ensure probability is between 0 and 1
  return(max(0, min(1, final_prob)))
}

# Run simulation for one step
simulate_step <- function(grid, temperature, wind_speed, wind_direction, humidity, veg_type) {
  new_grid <- grid
  
  # Find all burning cells
  burning_cells <- which(grid == 2, arr.ind = TRUE)
  
  # Process each burning cell
  if (nrow(burning_cells) > 0) {
    for (cell_idx in 1:nrow(burning_cells)) {
      i <- burning_cells[cell_idx, 1]
      j <- burning_cells[cell_idx, 2]
      
      # Burning cell becomes burned
      new_grid[i, j] <- 3
      
      # Check neighbors for potential spread
      for (di in -1:1) {
        for (dj in -1:1) {
          if (di == 0 && dj == 0) next  # Skip the center cell
          
          ni <- i + di
          nj <- j + dj
          
          # Check grid boundaries
          if (ni >= 1 && ni <= grid_size && nj >= 1 && nj <= grid_size) {
            # If neighbor has unburned vegetation
            if (grid[ni, nj] == 1) {
              # Calculate spread probability
              spread_prob <- calc_spread_probability(
                temperature, wind_speed, wind_direction, humidity, veg_type,
                i, j, ni, nj
              )
              
              # Random chance for fire to spread
              if (runif(1) < spread_prob) {
                new_grid[ni, nj] <- 2  # Set neighbor on fire
              }
            }
          }
        }
      }
    }
  }
  
  return(new_grid)
}

# Plot grid as heatmap
plot_grid <- function(grid, step, veg_type) {
  # Create color scheme for different cell states
  cell_colors <- c(
    "0" = "#e5e5e5",  # No vegetation - light gray
    "1" = if(veg_type == "grass") "#90ee90" else if(veg_type == "brush") "#9acd32" else if(veg_type == "forest") "#228b22" else "#8bc34a",  # Unburned vegetation - green variations
    "2" = "#ff4500",  # Burning - orange-red
    "3" = "#696969"   # Burned - dark gray
  )
  
  # Convert matrix to data frame for ggplot
  grid_df <- melt(grid)
  colnames(grid_df) <- c("x", "y", "state")
  grid_df$state <- as.factor(grid_df$state)
  
  # Plot
  plt <- ggplot(grid_df, aes(x = y, y = -x, fill = state)) +
    geom_tile() +
    scale_fill_manual(values = cell_colors,
                      labels = c("No Vegetation", "Unburned", "Burning", "Burned")) +
    labs(title = paste("Wildfire Simulation - Step", step),
         subtitle = paste("Vegetation Type:", veg_type),
         fill = "State") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 1)
  
  return(plt)
}

# Run full simulation and return time series data
run_simulation <- function(vegetation_density = 0.7,
                           temperature = 30,       # Celsius
                           wind_speed = 15,        # km/h
                           wind_direction = 90,    # degrees (0 = North, 90 = East)
                           humidity = 30,          # percentage
                           veg_type = "mixed") {   # grass, brush, forest, mixed
  
  # Initialize the grid
  grid <- initialize_grid(vegetation_density)
  
  # Store all grids for animation
  grid_history <- list()
  grid_history[[1]] <- grid
  
  # Store time series data
  time_series <- data.frame(
    step = 0,
    unburned = sum(grid == 1),
    burning = sum(grid == 2),
    burned = sum(grid == 3),
    total_veg = sum(grid > 0)
  )
  
  # Run simulation steps
  for (step in 1:num_steps) {
    grid <- simulate_step(grid, temperature, wind_speed, wind_direction, humidity, veg_type)
    grid_history[[step + 1]] <- grid
    
    # Update time series data
    time_series <- rbind(time_series, data.frame(
      step = step,
      unburned = sum(grid == 1),
      burning = sum(grid == 2),
      burned = sum(grid == 3),
      total_veg = sum(grid > 0)
    ))
    
    # Stop if fire is no longer burning
    if (sum(grid == 2) == 0 && step > 1) {
      break
    }
  }
  
  # Calculate percentages for plotting
  time_series <- time_series %>%
    mutate(
      pct_unburned = unburned / total_veg * 100,
      pct_burning = burning / total_veg * 100,
      pct_burned = burned / total_veg * 100
    )
  
  return(list(grid_history = grid_history, time_series = time_series, veg_type = veg_type))
}

# Plot time series data
plot_time_series <- function(time_series) {
  # Reshape data for plotting
  ts_plot <- time_series %>%
    select(step, pct_unburned, pct_burning, pct_burned) %>%
    reshape2::melt(id.vars = "step")
  
  # Create the time series plot
  plt <- ggplot(ts_plot, aes(x = step, y = value, color = variable)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("pct_unburned" = "#8bc34a", "pct_burning" = "#ff4500", "pct_burned" = "#696969"),
      labels = c("Unburned", "Burning", "Burned"),
      name = "Vegetation State"
    ) +
    labs(
      title = "Wildfire Progression Over Time",
      x = "Simulation Step",
      y = "Percentage of Total Vegetation (%)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(plt)
}

# Compare different scenarios
compare_scenarios <- function() {
  # Default parameters
  veg_density <- 0.7
  temp <- 30
  wind_spd <- 15
  wind_dir <- 90
  humid <- 30
  
  # Run simulations with different conditions
  scenario1 <- run_simulation(
    vegetation_density = veg_density,
    temperature = temp,
    wind_speed = wind_spd,
    wind_direction = wind_dir,
    humidity = humid,
    veg_type = "grass"
  )
  
  scenario2 <- run_simulation(
    vegetation_density = veg_density,
    temperature = temp,
    wind_speed = wind_spd,
    wind_direction = wind_dir,
    humidity = humid,
    veg_type = "forest"
  )
  
  scenario3 <- run_simulation(
    vegetation_density = veg_density,
    temperature = 40,  # Higher temperature
    wind_speed = wind_spd,
    wind_direction = wind_dir,
    humidity = 10,     # Lower humidity
    veg_type = "mixed"
  )
  
  # Prepare data for comparison plot
  s1 <- scenario1$time_series %>% 
    select(step, pct_burned) %>%
    mutate(scenario = "Grassland")
  
  s2 <- scenario2$time_series %>% 
    select(step, pct_burned) %>%
    mutate(scenario = "Forest")
  
  s3 <- scenario3$time_series %>% 
    select(step, pct_burned) %>%
    mutate(scenario = "Hot & Dry (Mixed Veg)")
  
  comparison_data <- rbind(s1, s2, s3)
  
  # Create comparison plot
  plt <- ggplot(comparison_data, aes(x = step, y = pct_burned, color = scenario)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = "Wildfire Spread Comparison Under Different Conditions",
      x = "Simulation Step",
      y = "Percentage of Vegetation Burned (%)",
      color = "Scenario"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(plt)
}

# Run a sample simulation
sim_results <- run_simulation(
  vegetation_density = 0.7,
  temperature = 32,
  wind_speed = 20,
  wind_direction = 90,  # East
  humidity = 25,
  veg_type = "mixed"
)

# Plot final state of the simulation
final_grid <- sim_results$grid_history[[length(sim_results$grid_history)]]
final_plot <- plot_grid(final_grid, length(sim_results$grid_history) - 1, sim_results$veg_type)
print(final_plot)

# Plot time series
ts_plot <- plot_time_series(sim_results$time_series)
print(ts_plot)

# Compare different scenarios
comparison_plot <- compare_scenarios()
print(comparison_plot)

# Example of how to run a custom simulation:
# custom_sim <- run_simulation(
#   vegetation_density = 0.8,  # Higher vegetation density
#   temperature = 38,          # Hot temperature
#   wind_speed = 25,           # Strong wind
#   wind_direction = 45,       # Northeast wind
#   humidity = 15,             # Very dry
#   veg_type = "brush"         # Brush/Shrub vegetation
# )
# 
# custom_plot <- plot_grid(custom_sim$grid_history[[length(custom_sim$grid_history)]], 
#                          length(custom_sim$grid_history) - 1, custom_sim$veg_type)
# print(custom_plot)
# 
# custom_ts <- plot_time_series(custom_sim$time_series)
# print(custom_ts)