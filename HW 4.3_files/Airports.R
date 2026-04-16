
#Load Data
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)

airports <- tibble(
  airport = c("ATL", "FRA", "PKX", "LAX", "DXB", "LHR"),
  `2020` = c(42.9, 18.8, 16.1, 28.8, 25.9, 22.1),
  `2021` = c(75.7, 24.8, 25.1, 48.0, 29.1, 19.4),
  `2022` = c(93.7, 48.9, 52.0, 66.0, 66.1, 61.6),
  `2023` = c(104.7, 59.4, 65.7, 75.1, 86.9, 79.2),
  `2024` = c(108.1, 61.6, 70.0, 80.0, 92.3, 84.1),
  `2025` = c(110.0, 63.0, 72.0, 82.0, 95.0, 88.0)
)

#Clean Data
airports_tidy <- airports %>%
  pivot_longer(
    cols = -airport,
    names_to = "year",
    values_to = "passengers_millions"
  ) %>%
  mutate(
    year = as.numeric(year)
  ) %>%
  arrange(airport, year)

#Create Table
airports_table <- airports_tidy %>%
  mutate(passengers_millions = round(passengers_millions, 1)) %>%
  pivot_wider(
    names_from = year,
    values_from = passengers_millions
  ) %>%
  arrange(airport)

kable(
  airports_table,
  col.names = c("Airport", "2020", "2021", "2022", "2023", "2024", "2025"),
  caption = "Passenger Traffic (Millions) for Selected Major Airports (2020–2025)"
)

# Create Plot
airport_plot = ggplot(
  data = airports_tidy, aes(x = year, y = passengers_millions, color = airport, group = airport)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Passenger Traffic Trends for Major Airports (2020–2025)",
    subtitle = "Comparing recovery and growth across six international airports",
    x = "Year",
    y = "Passengers (Millions)",
    color = "Airport"
  ) +
  scale_x_continuous(breaks = 2020:2025) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )