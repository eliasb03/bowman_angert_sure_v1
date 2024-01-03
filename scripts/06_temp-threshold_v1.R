##################################################
# Temperature Threshold
##################################################
# Script: 06_temp-threshold_v1.R
# Purpose: This code is going to create two methods of determining heat stress as time above a threshold
#       One method will simple sum the amount of time above the threshold
#       The second method will create an integral of the time spent above the threshold
# Author: Elias Bowman
# Date: 2023-08-29
# Version: 1.0
##################################################
## Specifying temperture threshold
threshold_temperature <- 32

## Method 1 - Sum
# Create a small dataframe, grouped by plot
# Calculates the amount of data above threshold
time_above_threshold <- hobo_data %>%
  group_by(plot) %>%
  summarize(total_time_above_threshold = sum(ifelse(ground_temp > threshold_temperature, 1, 0)))

## Method 2 - Integral
# Create a small dataframe, grouped by plot
# Calculates essentially riemann sum of the temperature data with respects to the the threshold
severity_above_threshold <- hobo_data %>%
  group_by(plot) %>%
  mutate(above_threshold = ifelse(ground_temp > threshold_temperature, ground_temp - threshold_temperature, 0)) %>%
  mutate(time_interval = lead(date.time) - date.time) %>%
  mutate(area = above_threshold * as.numeric(time_interval)) %>%
  summarize(heat_severity = sum(area, na.rm = TRUE)) 

