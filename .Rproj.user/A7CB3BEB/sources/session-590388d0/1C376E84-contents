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