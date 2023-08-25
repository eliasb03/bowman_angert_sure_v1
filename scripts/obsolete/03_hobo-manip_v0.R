##################################################
# HOBO Data Manipulations and Summaries
##################################################
# Script: 03_hobo_manip_v0.R
# Purpose: This code is used to manipulate HOBO data and plot attributes, it will be useful for working 
#   with hobo_data dataframe later on
# Author: Elias Bowman
# Date: 2023-08-011
# Version: 0.0
##################################################

### Prepare relevant functions for hobo dataframe work ###
## Function to return all HOBO Data for a plot
get_hobo_plot <- function(plot.title){
  temp_hobo_data <- filter(hobo_data, plot == plot.title)
  return(temp_hobo_data)
}

## Function to return single days data from a hobo dataframe
get_hobo_day <- function(df, date){
  filter_date <- as.Date(date)
  start_timestamp <- as.POSIXct(paste(filter_date, "00:00:00"))
  end_timestamp <- as.POSIXct(paste(filter_date, "23:59:59"))
  temp_hobo_data1 <- df %>% 
    filter(date.time >= start_timestamp & date.time <= end_timestamp)
}

### Fill in a summary data for hobo data ###
# Function to return mean daily temperature for a plot and a date


# calculate_mean_daily_temp <- function(df, plot, date) {
#   temp_hobo_data <- get_hobo_plot(plot)
#   temp_hobo_day <- get_hobo_day(temp_hobo_data, date)
#   mean_temp <- mean(temp_hobo_day$date.time, na.rm = TRUE)
#   return(mean_temp)
# }
# 
# hobo_summary <- hobo_summary %>%
#   mutate(mean_daily_temperature = calculate_mean_daily_temp(hobo_data, first(plot), date))
# 
## Generate Mean Daily Temperatures for each plot
## Generate Average Daily High Temperatures
## Generate Daily Max Temperature
## Generate Daily Min Temperature
## Generate Average Temperature Range

hobo_summary <- hobo_data %>%
   group_by(plot, date) %>%
   summarize(
     mean_temp = mean(ground_temp),
     max_temp = max(ground_temp),
     min_temp = min(ground_temp),
     temp_range = max_temp - min_temp, # The overall temp range the sites experience
   ) %>%
   ungroup() %>%
   group_by(plot) %>%
   summarize(
     mean_daily_temp = mean(mean_temp),
     mean_daily_max = mean(max_temp),
     mean_daily_min = mean(min_temp),
     mean_daily_temp_range = mean(max_temp - min_temp)
   ) %>%
   ungroup() %>%
   mutate(treatment = stringr::str_trunc(hobo_summary$plot, width = 3, ellipsis = ""))


