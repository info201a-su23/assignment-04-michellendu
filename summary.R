# AUTHOR: Michelle Ndugulile
# This script summarizes data calculations from a small prison in WA state

# Load packages
library(dplyr)
library(ggplot2)
library(maps)
library(scales)

# Retrieve small_prison_jail_data_wa csv data file
small_prison_jail_data_wa <- read_csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv")

# Calculate the average value of black jail population rate across all the counties

average_black_population_rate <- mean(
  small_prison_jail_data_wa$black_jail_pop_rate, na.rm = TRUE)
print(average_black_population_rate)

# In which county is the black jail population rate the highest?

county_with_highest_rate <- small_prison_jail_data_wa %>%
  filter(black_jail_pop_rate == max(black_jail_pop_rate, na.rm = TRUE)) %>%
  pull(county_name)
print(county_with_highest_rate)


# In which county is the black jail population rate the lowest?

county_with_lowest_rate <- small_prison_jail_data_wa %>%
  filter(black_jail_pop_rate == min(black_jail_pop_rate, na.rm = TRUE)) %>%
  pull(county_name)
print(county_with_lowest_rate)

# Which year had the highest black jail population rate?

year_with_highest_rate <- small_prison_jail_data_wa %>%
  filter(black_jail_pop_rate == max(black_jail_pop_rate, na.rm = TRUE)) %>%
  pull(year)
print(year_with_highest_rate)

# What is the median black jail population rate across all counties? 

median_black_population_rate <- median(
  small_prison_jail_data_wa$black_jail_pop_rate, na.rm = TRUE)
print(median_black_population_rate)

# Trends over time chart indicating Black Jail Population Rate in Five Counties of WA.

filtered_data <- small_prison_jail_data_wa %>%
  filter(county_name %in% c("King County", "Pierce County", "Snohomish County", "Spokane County", "Clark County")) %>%
  arrange(year)

ggplot(filtered_data, aes(x = year, y = black_jail_pop_rate, color = county_name)) +
  geom_line() +
  labs(
    title = "Trend of Black Jail Population Rate in Five Counties of WA",
    x = "Year",
    y = "Black Jail Population Rate",
    color = "County"
  )


# Plot a graph comparing the two variables, Black vs. White jail population rates by year.

filtered_data <- small_prison_jail_data_wa %>%
  arrange(year)
ggplot(filtered_data, aes(x = year)) +
  geom_point(aes(y = black_jail_pop_rate, color = "Black")) +
  geom_point(aes(y = white_jail_pop_rate, color = "White")) +
  labs(
    title = "Comparison of Black and White Jail Population Rates by Year",
    x = "Year",
    y = "Jail Population Rate",
    color = "Race"
  ) +
  scale_color_manual(values = c("Black" = "blue", "White" = "red")) 

# Create a map to indicate the Black jail population rates in Washington state counties.

state_shape <- read_sf("/Users/michellendugulile/Desktop/INFO201/assignment-04-michellendu/WA_State_Boundary.shp")
black_jail_pop_state_shape <- left_join(small_prison_jail_data_wa, state_shape, by = c("state" = "region"))

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )

black_population_map <- ggplot(data = black_jail_pop_state_shape) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate)) +
  scale_fill_continuous(name = "Black Jail Population Rate", low = "lightblue", high = "darkblue") +
  labs(title = "Black Jail Population Rates in Washington State Counties", fill = "Black Jail Population Rate") +
  coord_map() +
  blank_theme

print(black_population_map)

