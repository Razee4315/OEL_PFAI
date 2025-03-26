# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)  # For working with dates

# Load the dataset
# Replace 'path_to_your_file' with your actual file path
climate_data <- read.csv("GlobalTemperatures.csv")

# Examine the dataset structure
head(climate_data)
str(climate_data)

# Data Preparation
# Convert dt (date) to Date type and extract year/month
climate_data <- climate_data %>%
  mutate(
    dt = as.Date(dt),
    year = year(dt),
    month = month(dt)
  ) %>%
  filter(!is.na(LandAverageTemperature))  # Remove NA values

# Create annual averages for cleaner visualizations
annual_avg <- climate_data %>%
  group_by(year) %>%
  summarise(
    avg_land_temp = mean(LandAverageTemperature, na.rm = TRUE),
    avg_land_ocean_temp = mean(LandAndOceanAverageTemperature, na.rm = TRUE)
  ) %>%
  filter(!is.na(avg_land_temp) & !is.na(avg_land_ocean_temp))




# Create a line chart comparing land vs land+ocean temperatures
temp_comparison <- ggplot(annual_avg, aes(x = year)) +
  geom_line(aes(y = avg_land_temp, color = "Land Only"), size = 1) +
  geom_line(aes(y = avg_land_ocean_temp, color = "Land + Ocean"), size = 1) +
  scale_color_manual(values = c("Land Only" = "darkgreen", "Land + Ocean" = "steelblue")) +
  labs(title = "Global Temperature Changes (1750-2015)",
       x = "Year",
       y = "Average Temperature (°C)",
       color = "Temperature Type",
       caption = "Source: Berkeley Earth Surface Temperature Data") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(1750, 2015, by = 25))

# Display and save the plot
print(temp_comparison)
ggsave("temperature_comparison_line_chart.png", plot = temp_comparison, 
       width = 10, height = 6, dpi = 300)




# Create a scatter plot showing temperature vs its uncertainty
# We'll use monthly data for more points
set.seed(123)  # For reproducibility when sampling
sample_data <- climate_data %>% 
  sample_n(1000)  # Take a sample for clearer visualization

uncertainty_plot <- ggplot(sample_data, 
                           aes(x = LandAverageTemperature, 
                               y = LandAverageTemperatureUncertainty)) +
  geom_point(aes(color = month), alpha = 0.7, size = 3) +
  scale_color_gradientn(colors = rainbow(12), 
                        breaks = 1:12, 
                        labels = month.abb) +
  labs(title = "Temperature Measurement Uncertainty",
       x = "Land Average Temperature (°C)",
       y = "Temperature Uncertainty",
       color = "Month",
       caption = "Source: Berkeley Earth Surface Temperature Data") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right"
  ) +
  geom_smooth(method = "lm", color = "black", se = FALSE)

# Display and save the plot
print(uncertainty_plot)
ggsave("temperature_uncertainty_scatter.png", plot = uncertainty_plot, 
       width = 10, height = 6, dpi = 300)