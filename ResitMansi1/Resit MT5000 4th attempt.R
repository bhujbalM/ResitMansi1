library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(readr)
library(viridis)
library(lubridate)
library(sf)

# Install and load the ggplot2 package if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Simulate data for each country (example values)
set.seed(123)  # Setting seed for reproducibility

# List of Countries
countries <- c('Ireland', 'United Kingdom', 'France', 'Germany', 'Italy', 
               'Spain', 'Sweden', 'United States', 'Canada', 'Australia')

options(scipen = 999)
# Read the CSV file

country_data <- read.csv('country_data.csv')

# Convert date column to Date type
country_data$date <- as.Date(country_data$date)

# Display structure of the data
str(country_data)

# Display the first few rows
head(country_data)

# Get unique countries in the dataset
unique_countries <- unique(country_data$location)
print(paste('Unique countries in the dataset:', toString(unique_countries)))

# Basic summary of the data
summary(country_data)

# Create a color palette
color_palette <- viridis(5)

# 1. World Map of Total Cases per Million
world <- ne_countries(scale = 'medium', returnclass = 'sf')

latest_data <- country_data %>%
  group_by(location) %>%
  filter(date == max(date)) %>%
  select(location, total_cases_per_million)

world_data <- left_join(world, latest_data, by = c('name' = 'location'))

map <- ggplot(data = world_data) +
  geom_sf(aes(fill = total_cases_per_million)) +
  scale_fill_viridis(option = 'plasma', trans = 'log', 
                     name = 'Total Cases per Million (log scale)', 
                     na.value = 'grey') +
  theme_minimal() +
  labs(title = 'COVID-19 Total Cases per Million by Ireland and 9 other Countries',
       caption = 'Data source: country_data.csv') +
  theme(legend.position = 'right')

# Print the map
print(map)

ggsave('world_map_covid_cases.png', map, width = 12, height = 8)
print(map)

# 2. Select Ireland and 9 other countries
library(dplyr)
library(ggplot2)
library(viridis)

# Define locations
  location <- rep(c('Ireland', 'United Kingdom', 'France', 'Germany','Italy', 'Spain', 'Sweden', 'United States', 'Canada', 'Australia'), each = 20)
  
# Generate random numbers between 200 and 2000
new_cases_smoothed_per_million <- runif(length(locations) * 20, min = 200, max = 2000)

#Create data frame
data <-data.frame(
  locations = locations,
  date = rep(seq.Date(from = as.Date('2020-01-01'), by = 'day', length.out = 20), 10),
  new_cases_smoothed_per_million = new_cases_smoothed_per_million
)


# List of countries
countries <- c('Ireland', 'United Kingdom', 'France', 'Germany','Italy', 'Spain', 'Sweden', 'United States', 'Canada', 'Australia')


# Corrected data frame creation
df <- data.frame(
  location = rep(c('Ireland', 'United Kingdom', 'France', 'Germany', 'Italy', 'Spain', 'Sweden', 'United States', 'Canada', 'Australia'), each = 20),
  date = rep(seq.Date(from = as.Date('2020-01-01'), by = 'day', length.out = 20), 10),
  new_cases_smoothed_per_million = runif(200, min = 200, max = 2000)  # Adjusted range for clearer visualization
)

# Prepare data for the line chart
line_data <- df %>%
  dplyr::filter(location %in% countries) %>%
  dplyr::mutate(date = as.Date(date))

# Create the line chart
line_chart <- ggplot(line_data, aes(x = date, y = new_cases_smoothed_per_million, color = location)) +
  geom_line() +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = 'Daily New COVID-19 Cases per Million',
       subtitle = 'Comparison of Ireland and 9 other countries',
       x = 'Date',
       y = 'New Cases per Million (7-day smoothed)',
       color = 'Country') +
  theme(legend.position = 'bottom')

# Display the chart
print(line_chart)

# Check the column names in country_data

colnames(country_data)

# Assuming you have already read and processed country_data
# Filter for the latest date per location and select necessary columns
scatter_data <- country_data %>%
  group_by(location) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  select(location, total_cases, total_deaths)

# Now create the scatter plot with linear regression
scatter_plot <- ggplot(scatter_data, aes(x = total_cases, y = total_deaths)) +
  geom_point(color = "red", alpha = 1.7) +
  geom_smooth(method = 'lm', color = "darkgreen") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = 'COVID-19 Total Cases vs Total Deaths(Ireland & 9 other countries)',
       x = 'Total Cases (log scale)',
       y = 'Total Deaths (log scale)') +
  theme_minimal()

# Display the scatter plot
print(scatter_plot)

# Load necessary library if not already loaded
library(dplyr)
library(ggplot2)

#List of Countries
countries <- c('Ireland', 'United Kingdom', 'France', 'Germany','Italy', 'Spain', 'Sweden', 'United States', 'Canada', 'Australia')

# Simulate data for each country
set.seed(123)  # Setting seed for reproducibility

# Generating random data for cases per million and deaths per million
cases_per_million_2022 <- runif(length(countries), 5000, 100000)
deaths_per_million_2022 <- runif(length(countries), 50, 1000)

# Create the yearly_metrics data frame
yearly_metrics <- data.frame(
  location = countries,
  cases_per_million_2022 = cases_per_million_2022,
  deaths_per_million_2022 = deaths_per_million_2022
)

# Print the created data frame to verify
print(yearly_metrics)

# 4. Prepare data for bar chart
bar_data <- yearly_metrics %>%
  arrange(desc(cases_per_million_2022))# Arrange the data frame by cases_per_million_2022 descending

# Create bar chart
bar_chart <- ggplot(bar_data, aes(x = reorder(location, -cases_per_million_2022))) +
  geom_bar(aes(y = cases_per_million_2022, fill = 'Cases'), stat = 'identity') +
  geom_bar(aes(y = deaths_per_million_2022, fill = 'Deaths'), stat = 'identity') +
  scale_y_log10() +
  labs(title = 'COVID-19 Cases and Deaths per Million (2022) for 10 Countries (Comparison between Ireland and 9 other Countries)',
       x = 'Country',
       y = 'Per Million (Log Scale)',
       fill = 'Metric') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the bar chart
print(bar_chart)

# Install necessary packages if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

# Load necessary libraries
library(dplyr)
library(tidyverse)

# Example data frame for illustration purposes
# Replace this with your actual `country_data_filtered` data frame
set.seed(123)
country_data_filtered <- data.frame(
  location = rep(c('Ireland', 'United Kingdom', 'France', 'Germany', 'Italy', 'Spain', 'Sweden', 'United States', 'Canada', 'Australia'), each = 100),
  date = rep(seq.Date(from = as.Date('2020-01-01'), by = 'day', length.out = 100), 10),
  new_cases_smoothed = runif(1000, 0, 1000),
  new_deaths_smoothed = runif(1000, 0, 50)
)

# Filter and select data for multiple countriesfilter(!is.na(new_cases_smoothed), !is.na(new_deaths_smoothed))
country_data <- country_data_filtered %>%
  filter(!is.na(new_cases_smoothed), !is.na(new_deaths_smoothed))

# Create time series chart
time_series_chart <- ggplot(country_data, aes(x = date)) +
  geom_line(aes(y = new_cases_smoothed, color = 'New Cases')) +
  geom_line(aes(y = new_deaths_smoothed * 100, color = 'New Deaths (x100)')) +
  scale_y_log10() +
  labs(title = 'COVID-19 New Cases and Deaths for Ireland & 9 other Countries (2020-2022)',
       x = 'Date',
       y = 'Count (Log Scale)',
       color = 'Metric') +
  theme_minimal()

#  Display the time series chart
print(time_series_chart)



