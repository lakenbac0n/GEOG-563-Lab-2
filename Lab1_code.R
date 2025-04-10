# simulate_predator_prey.R

# Parameters
prey_init <- 40       # Initial prey population
pred_init <- 9        # Initial predator population
a <- 0.1              # Prey growth rate
b <- 0.02             # Predation rate
c <- 0.1              # Predator death rate
d <- 0.01             # Predator reproduction rate per prey eaten
dt <- 0.1             # Time step
time_end <- 200       # Total time
n_steps <- time_end / dt

# Initialize time and population vectors
time <- seq(0, time_end, by = dt)
prey <- numeric(length(time))
pred <- numeric(length(time))
prey[1] <- prey_init
pred[1] <- pred_init

# Euler integration loop
for (t in 2:length(time)) {
  prey[t] <- prey[t - 1] + (a * prey[t - 1] - b * prey[t - 1] * pred[t - 1]) * dt
  pred[t] <- pred[t - 1] + (d * prey[t - 1] * pred[t - 1] - c * pred[t - 1]) * dt
}

# Save results
data <- data.frame(time = time, prey = prey, predator = pred)
write.csv(data[, c("time", "prey")], "prey_timeseries.csv", row.names = FALSE)
write.csv(data[, c("time", "predator")], "predator_timeseries.csv", row.names = FALSE)
# simulate_polar_bears_seals.R
# Lotka-Volterra model for polar bears (predator) and seals (prey)

# Parameters - adjusted for polar bear and seal dynamics
seals_init <- 1000      # Initial seal population
bears_init <- 50        # Initial polar bear population
a <- 0.3                # Seal growth rate (higher than generic model due to faster reproduction)
b <- 0.003              # Predation rate (lower as polar bears hunt less frequently than smaller predators)
c <- 0.15               # Polar bear death rate (relatively higher due to harsh Arctic conditions)
d <- 0.005              # Polar bear reproduction rate per seal eaten (lower conversion efficiency)
dt <- 0.1               # Time step
time_end <- 300         # Total time (extended to see multiple cycles)
n_steps <- time_end / dt

# Initialize time and population vectors
time <- seq(0, time_end, by = dt)
seals <- numeric(length(time))
bears <- numeric(length(time))
seals[1] <- seals_init
bears[1] <- bears_init

# Euler integration loop
for (t in 2:length(time)) {
  seals[t] <- seals[t - 1] + (a * seals[t - 1] - b * seals[t - 1] * bears[t - 1]) * dt
  bears[t] <- bears[t - 1] + (d * seals[t - 1] * bears[t - 1] - c * bears[t - 1]) * dt
  
  # Add ecological constraints (populations can't be negative)
  seals[t] <- max(0, seals[t])
  bears[t] <- max(0, bears[t])
}

# Save results
data <- data.frame(time = time, seals = seals, polar_bears = bears)
write.csv(data[, c("time", "seals")], "seals_timeseries.csv", row.names = FALSE)
write.csv(data[, c("time", "polar_bears")], "polar_bears_timeseries.csv", row.names = FALSE)

# Population over time
p1 <- ggplot(data, aes(x = time)) +
    geom_line(aes(y = seals, color = "Seals"), size = 1) +
    geom_line(aes(y = polar_bears * 10, color = "Polar Bears (×10)"), size = 1) +
    scale_color_manual(values = c("Seals" = "blue", "Polar Bears (×10)" = "red")) +
    labs(title = "Polar Bear and Seal Population Dynamics",
         x = "Time", y = "Population Size",
         color = "Species") +
    theme_minimal()
  
  # Save plot
  ggsave("polar_bear_seal_dynamics.png", p1, width = 8, height = 5)
  
  # Phase portrait
p2 <- ggplot(data, aes(x = seals, y = polar_bears)) +
    geom_path(color = "purple") +
    geom_point(data = data[1,], aes(x = seals, y = polar_bears), 
               color = "green", size = 3) +
    labs(title = "Phase Portrait: Polar Bears vs Seals",
         x = "Seal Population", y = "Polar Bear Population") +
    theme_minimal()
  
  # Save phase portrait
  ggsave("phase_portrait.png", p2, width = 7, height = 7)
  
insta
library(ggplot2)
# simulate_polar_bears_seals.R
# Lotka-Volterra model for polar bears (predator) and seals (prey)

# Load required library
library(ggplot2)

# Parameters - adjusted for polar bear and seal dynamics
seals_init <- 1000      # Initial seal population
bears_init <- 50        # Initial polar bear population
a <- 0.3                # Seal growth rate (higher than generic model due to faster reproduction)
b <- 0.003              # Predation rate (lower as polar bears hunt less frequently than smaller predators)
c <- 0.15               # Polar bear death rate (relatively higher due to harsh Arctic conditions)
d <- 0.005              # Polar bear reproduction rate per seal eaten (lower conversion efficiency)
dt <- 0.1               # Time step
time_end <- 300         # Total time (extended to see multiple cycles)
n_steps <- time_end / dt

# Initialize time and population vectors
time <- seq(0, time_end, by = dt)
seals <- numeric(length(time))
bears <- numeric(length(time))
seals[1] <- seals_init
bears[1] <- bears_init

# Euler integration loop
for (t in 2:length(time)) {
  seals[t] <- seals[t - 1] + (a * seals[t - 1] - b * seals[t - 1] * bears[t - 1]) * dt
  bears[t] <- bears[t - 1] + (d * seals[t - 1] * bears[t - 1] - c * bears[t - 1]) * dt
  
  # Add ecological constraints (populations can't be negative)
  seals[t] <- max(0, seals[t])
  bears[t] <- max(0, bears[t])
}

# Save results
data <- data.frame(time = time, seals = seals, polar_bears = bears)
write.csv(data[, c("time", "seals")], "seals_timeseries.csv", row.names = FALSE)
write.csv(data[, c("time", "polar_bears")], "polar_bears_timeseries.csv", row.names = FALSE)

# Create long format data for easier plotting
data_long <- reshape2::melt(data, id.vars = "time", 
                            variable.name = "species", 
                            value.name = "population")

# Plot 1: Population over time
p1 <- ggplot(data_long, aes(x = time, y = population, color = species)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("seals" = "blue", "polar_bears" = "red"),
                     labels = c("Seals", "Polar Bears")) +
  labs(title = "Polar Bear and Seal Population Dynamics",
       subtitle = "Lotka-Volterra Model Simulation",
       x = "Time", 
       y = "Population Size",
       color = "Species") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# Plot 2: Population over time with different scales (dual y-axis effect)
# This is a better way to view the different population sizes
install.packages("ggplot2")
library(ggplot2)

p2 <- ggplot(data) +
  geom_line(aes(x = time, y = seals, color = "Seals"), size = 1) +
  geom_line(aes(x = time, y = polar_bears * 20, color = "Polar Bears (×20)"), size = 1) +
  scale_color_manual(values = c("Seals" = "blue", "Polar Bears (×20)" = "red")) +
  labs(title = "Polar Bear and Seal Population Dynamics",
       subtitle = "With scaled polar bear population for visibility",
       x = "Time", 
       y = "Population Size",
       color = "Species") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# Plot 3: Phase portrait
p3 <- ggplot(data, aes(x = seals, y = polar_bears)) +
  geom_path(color = "purple", alpha = 0.7) +
  geom_point(data = data[1,], aes(x = seals, y = polar_bears), 
             color = "green", size = 3) +
  geom_point(data = data[nrow(data),], aes(x = seals, y = polar_bears), 
             color = "red", size = 3) +
  annotate("text", x = data$seals[1] + 50, y = data$polar_bears[1], 
           label = "Start", color = "green4") +
  labs(title = "Phase Portrait: Polar Bears vs Seals",
       subtitle = "Cyclical relationship between predator and prey populations",
       x = "Seal Population", 
       y = "Polar Bear Population") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# Plot 4: Combined plot showing both populations and their ratio
p4 <- ggplot(data) +
  geom_line(aes(x = time, y = seals, color = "Seals"), size = 1) +
  geom_line(aes(x = time, y = polar_bears * 20, color = "Polar Bears (×20)"), size = 1) +
  geom_line(aes(x = time, y = seals/polar_bears, color = "Seal:Bear Ratio"), size = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("Seals" = "blue", "Polar Bears (×20)" = "red", "Seal:Bear Ratio" = "darkgreen")) +
  labs(title = "Polar Bear and Seal Population Dynamics with Ratio",
       x = "Time", 
       y = "Population Size / Ratio",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# Display plots
print(p1)
print(p2)
print(p3)
print(p4)

# Save plots
ggsave("polar_bear_seal_dynamics_basic.png", p1, width = 10, height = 6)
ggsave("polar_bear_seal_dynamics_scaled.png", p2, width = 10, height = 6)
ggsave("phase_portrait.png", p3, width = 8, height = 8)
ggsave("population_with_ratio.png", p4, width = 10, height = 6)

# Create a multi-panel figure
install.packages("gridExtra")
library(gridExtra)
combined_plot <- grid.arrange(p2, p3, ncol = 2)
ggsave("combined_plots.png", combined_plot, width = 14, height = 7)
