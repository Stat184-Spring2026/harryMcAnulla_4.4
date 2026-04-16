# Monte Carlo simulation function
mc_simulate <- function(n, x_min, x_max, y_min, y_max) {
  
  # Generate random x and y values
  x_vals <- runif(n, min = x_min, max = x_max)
  y_vals <- runif(n, min = y_min, max = y_max)
  
  # Combine into a data frame
  sim_data <- data.frame(
    x = x_vals,
    y = y_vals
  )
  
  return(sim_data)
}

# Load libraries
library(dplyr)
library(ggplot2)

run_simulation <- function(n){

# Bounds
x_min <- -3
x_max <- 3
y_min <- 0
y_max <- 0.5

# Rectangle
rect_area <- (x_max - x_min) * (y_max - y_min)
n <- 1000

points_df <- mc_simulate(n, x_min, x_max, y_min, y_max)

f <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}

# Classify points and estimate area
points_classified <- points_df %>%
  mutate(
    f_x = f(x),
    inside = ifelse(y <= f_x, "Inside", "Outside")
  )

# Calculate estimate
prop_inside <- mean(points_classified$inside == "Inside")
estimated_area <- rect_area * prop_inside

curve_df <- data.frame(
  x = seq(x_min, x_max, length.out = 500)
) %>%
  mutate(y = f(x))

# Plot
p <- ggplot() +
  geom_point(
    data = points_classified,
    aes(x = x, y = y, color = inside),
    alpha = 0.6,
    size = 1.5
  ) +
  geom_line(
    data = curve_df,
    aes(x = x, y = y),
    size = 1
  ) +
  labs(
    title = paste("Monte Carlo Integration (n =", n, ")"),
    subtitle = paste("Estimated Area =", round(estimated_area, 4)),
    x = "x",
    y = "y",
    color = "Point Type"
  ) +
  xlim(x_min, x_max) +
  ylim(y_min, y_max) +
  theme_minimal()

return(p)
}

