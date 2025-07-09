# Compare air and water temp at two locations

library(tidyverse)
library(lubridate)


# Load required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)

# Read the CSV files
air_temp <- read.csv("Golden-perch-recruitment/Data/temps/Barwon_air_temp_-30.05_148.05(1).csv", stringsAsFactors = FALSE)
water_temp <- read.csv("Golden-perch-recruitment/Data/temps/Barwon water temps.csv", stringsAsFactors = FALSE)

# Clean and process air temperature data
air_temp_clean <- air_temp %>%
  # Convert date column to proper date format
  mutate(date = ymd(YYYY.MM.DD)) %>%
  # Calculate daily mean temperature
  mutate(daily_mean_air = (max_temp + min_temp) / 2) %>%
  # Select relevant columns
  select(date, daily_mean_air) %>%
  # Remove any rows with missing dates
  filter(!is.na(date)) %>%
  # Sort by date
  arrange(date)

# Clean and process water temperature data
water_temp_clean <- water_temp %>%
  # Skip header rows and process the data
  filter(!is.na(Mean) & Mean != "" & Mean != "Mean") %>%
  # Convert date column to proper date format
  mutate(date = as.Date(dmy_hm(paste(Date.and.time)))) %>%
  # Convert Mean to numeric (it should be water temperature)
  mutate(water_temp = as.numeric(Mean)) %>%
  # Select relevant columns
  select(date, water_temp) %>%
  # Remove any rows with missing data
  filter(!is.na(date) & !is.na(water_temp)) %>%
  # Sort by date
  arrange(date)

# Create a complete date sequence and merge datasets
# Find overlapping date range
start_date <- #as.Date("2000-01-01")
  max(min(air_temp_clean$date, na.rm = TRUE), 
                  min(water_temp_clean$date, na.rm = TRUE))
end_date <- #as.Date("2015-12-31")
              min(max(air_temp_clean$date, na.rm = TRUE), 
                max(water_temp_clean$date, na.rm = TRUE))

# Create complete date sequence
date_seq <- data.frame(date = seq(start_date, end_date, by = "day"))

# Merge all datasets
combined_data <- date_seq %>%
  left_join(air_temp_clean, by = "date") %>%
  left_join(water_temp_clean, by = "date")

# Calculate 3-month (90-day) rolling averages
combined_data <- combined_data %>%
  arrange(date) %>%
  mutate(
    air_temp_90day = rollmean(daily_mean_air, k = 90, fill = NA, align = "right"),
    water_temp_90day = rollmean(water_temp, k = 90, fill = NA, align = "right")
  )

# Filter to complete cases for correlation analysis
correlation_data <- combined_data %>%
  filter(!is.na(air_temp_90day) & !is.na(water_temp_90day))

# Calculate correlation
if(nrow(correlation_data) > 0) {
  correlation_coef <- cor(correlation_data$air_temp_90day, 
                          correlation_data$water_temp_90day, 
                          use = "complete.obs")
  
  cat("Correlation between 3-month rolling averages:\n")
  cat("Correlation coefficient:", round(correlation_coef, 4), "\n")
  cat("Number of overlapping observations:", nrow(correlation_data), "\n")
  cat("Date range:", format(min(correlation_data$date), "%Y-%m-%d"), 
      "to", format(max(correlation_data$date), "%Y-%m-%d"), "\n\n")
} else {
  cat("No overlapping data found for correlation analysis.\n")
}

# Create visualization
if(nrow(correlation_data) > 0) {
  # Time series plot
  p1 <- ggplot(correlation_data, aes(x = date)) +
    geom_line(aes(y = air_temp_90day, color = "Air Temperature"), size = 1) +
    geom_line(aes(y = water_temp_90day, color = "Water Temperature"), size = 1) +
    labs(title = "3-Month Rolling Averages: Air vs Water Temperature",
         subtitle = paste("Correlation coefficient:", round(correlation_coef, 4)),
         x = "Date",
         y = "Temperature (°C)",
         color = "Temperature Type") +
    theme_minimal() +
    scale_color_manual(values = c("Air Temperature" = "red", "Water Temperature" = "blue"))
  
  print(p1)
  
  # Scatter plot
  p2 <- ggplot(correlation_data, aes(x = air_temp_90day, y = water_temp_90day)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = "Correlation: Air vs Water Temperature (3-Month Rolling Averages)",
         subtitle = paste("R =", round(correlation_coef, 4)),
         x = "Air Temperature 90-day Rolling Average (°C)",
         y = "Water Temperature 90-day Rolling Average (°C)") +
    theme_minimal()
  
  print(p2)
}

### Now Darling
# Read the CSV files
air_temp <- read.csv("Golden-perch-recruitment/Data/temps/Darling3_air_temp_-33.75_142.25.csv", stringsAsFactors = FALSE)
water_temp <- read.csv("Golden-perch-recruitment/Data/temps/Darling3 water temp.csv", stringsAsFactors = FALSE)

# Clean and process air temperature data
air_temp_clean <- air_temp %>%
  # Convert date column to proper date format
  mutate(date = ymd(YYYY.MM.DD)) %>%
  # Calculate daily mean temperature
  mutate(daily_mean_air = (max_temp + min_temp) / 2) %>%
  # Select relevant columns
  select(date, daily_mean_air) %>%
  # Remove any rows with missing dates
  filter(!is.na(date)) %>%
  # Sort by date
  arrange(date)

# Clean and process water temperature data
water_temp_clean <- water_temp %>%
  # Skip header rows and process the data
  filter(!is.na(Mean) & Mean != "" & Mean != "Mean") %>%
  # Convert date column to proper date format
  mutate(date = as.Date(dmy_hm(paste(Date.and.time)))) %>%
  # Convert Mean to numeric (it should be water temperature)
  mutate(water_temp = as.numeric(Mean)) %>%
  # Select relevant columns
  select(date, water_temp) %>%
  # Remove any rows with missing data
  filter(!is.na(date) & !is.na(water_temp)) %>%
  # Sort by date
  arrange(date)

# Create a complete date sequence and merge datasets
# Find overlapping date range
start_date <- #as.Date("2000-01-01")
  max(min(air_temp_clean$date, na.rm = TRUE), 
      min(water_temp_clean$date, na.rm = TRUE))
end_date <- #as.Date("2015-12-31")
  min(max(air_temp_clean$date, na.rm = TRUE), 
      max(water_temp_clean$date, na.rm = TRUE))

# Create complete date sequence
date_seq <- data.frame(date = seq(start_date, end_date, by = "day"))

# Merge all datasets
combined_data <- date_seq %>%
  left_join(air_temp_clean, by = "date") %>%
  left_join(water_temp_clean, by = "date")

# Calculate 3-month (90-day) rolling averages
combined_data <- combined_data %>%
  arrange(date) %>%
  mutate(
    air_temp_90day = rollmean(daily_mean_air, k = 90, fill = NA, align = "right"),
    water_temp_90day = rollmean(water_temp, k = 90, fill = NA, align = "right")
  )

# Filter to complete cases for correlation analysis
correlation_data <- combined_data %>%
  filter(!is.na(air_temp_90day) & !is.na(water_temp_90day))

cor.test(correlation_data$air_temp_90day, 
                        correlation_data$water_temp_90day, 
                        use = "complete.obs")

# Calculate correlation
if(nrow(correlation_data) > 0) {
  correlation_coef <- cor(correlation_data$air_temp_90day, 
                          correlation_data$water_temp_90day, 
                          use = "complete.obs")
  
  cat("Correlation between 3-month rolling averages:\n")
  cat("Correlation coefficient:", round(correlation_coef, 4), "\n")
  cat("Number of overlapping observations:", nrow(correlation_data), "\n")
  cat("Date range:", format(min(correlation_data$date), "%Y-%m-%d"), 
      "to", format(max(correlation_data$date), "%Y-%m-%d"), "\n\n")
} else {
  cat("No overlapping data found for correlation analysis.\n")
}

# Create visualization
if(nrow(correlation_data) > 0) {
  # Time series plot
  p1 <- ggplot(correlation_data, aes(x = date)) +
    geom_line(aes(y = air_temp_90day, color = "Air Temperature"), size = 1) +
    geom_line(aes(y = water_temp_90day, color = "Water Temperature"), size = 1) +
    labs(title = "3-Month Rolling Averages: Air vs Water Temperature",
         subtitle = paste("Correlation coefficient:", round(correlation_coef, 4)),
         x = "Date",
         y = "Temperature (°C)",
         color = "Temperature Type") +
    theme_minimal() +
    scale_color_manual(values = c("Air Temperature" = "red", "Water Temperature" = "blue"))
  
  print(p1)
  
  # Scatter plot
  p2 <- ggplot(correlation_data, aes(x = air_temp_90day, y = water_temp_90day)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = "Correlation: Air vs Water Temperature (3-Month Rolling Averages)",
         subtitle = paste("R =", round(correlation_coef, 4)),
         x = "Air Temperature 90-day Rolling Average (°C)",
         y = "Water Temperature 90-day Rolling Average (°C)") +
    theme_minimal()
  
  print(p2)
}
