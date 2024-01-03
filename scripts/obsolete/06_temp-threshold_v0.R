##################################################
# Temperature Threshold
##################################################
# Script: 06_temp-threshold_v0.R
# Purpose: This code is going to create two methods of determining heat stress as time above a threshold
#       One method will simple sum the amount of time above the threshold
#       The second method will create an integral of the time spent above the threshold
# Author: Elias Bowman
# Date: 2023-08-29
# Version: 0.0
##################################################

threshold_temperature <- 35
## Method 1 - Sum
time_above_threshold <- hobo_data %>%
  group_by(plot) %>%
  summarize(total_time_above_threshold = sum(ifelse(ground_temp > threshold_temperature, 1, 0)))

## Method 2 - Integral
severity_above_threshold <- hobo_data %>%
  group_by(plot) %>%
  mutate(above_threshold = ifelse(ground_temp > threshold_temperature, ground_temp - threshold_temperature, 0)) %>%
  mutate(time_interval = lead(date.time) - date.time) %>%
  mutate(area = above_threshold * as.numeric(time_interval)) %>%
  summarize(heat_severity = sum(area, na.rm = TRUE)) 

# %>% left_join(hobo_data %>% select(plot, date.time))


# severity_above_threshold <- hobo_data %>%
#   group_by(plot) %>%
#   mutate(above_threshold = ifelse(ground_temp > threshold_temperature, ground_temp - threshold_temperature, 0)) %>%
#   mutate(time_interval = lead(date.time) - date.time) %>%
#   mutate(area = above_threshold * as.numeric(time_interval)) %>%
#   summarize(total_area = sum(area, na.rm = TRUE)) %>%
#   mutate(plot_area = total_area + threshold_temperature * as.numeric(max(date.time) - min(date.time))) %>%
#   ungroup()

