##################################################
# HOBO Data Manipulations and Summaries
##################################################
# Script: 03_hobo_manip_v1.R
# Purpose: This code is used to manipulate HOBO data and plot attributes, it will be useful for working 
#   with hobo_data dataframe later on
# Author: Elias Bowman
# Date: 2023-08-011
# Version: 1.0
#######################
### Prepare relevant functions for hobo dataframe work
##################################################
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

#######################
### Fill in a summary data for hobo data
## Generate for each instance: 
# Mean Daily Temperatures
# Average Daily High Temperatures
# Daily Max Temperature
# Daily Min Temperature
# Average Daily Temperature Range
# Time above Threshold Temp
#######################
## Dataframe containing summary statistics of mean daily variables sorted by plot
##################################################
hb.sum_by.plot <- data.frame()
hb.sum_by.plot <- hobo_data %>%
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
    mean_daily_temp_range = mean(max_temp - min_temp),
    count = n()
  ) %>%
  ungroup() %>%
  mutate(plot_treatment = substr(plot, 1, 3))

#######################
## Dataframe containing summary statistics of mean daily variables sorted by treatment group
##################################################
# Not the most useful or necessary table, but created initially just for assessment
hb.sum_by.treatment <- hb.sum_by.plot %>%
  group_by(plot_treatment) %>%
  summarize(
    tr_mean_temp = mean(mean_daily_temp),
    tr_mean_max_temp = max(mean_daily_max),
    tr_mean_min_temp = min(mean_daily_min),
    tr_mean_daily_range = mean(tr_mean_max_temp - tr_mean_min_temp),
    count = n()
  )

#######################
## Dataframe containing summary statistics of mean daily variables sorted by date
##################################################
# Different than hobo_summary, as it deals with hourly data
# includes daily: max temp, min temp, temp range, mean temp, and standard deviation of temp
hb.sum_by.day <- hobo_data %>%
  group_by(plot, date) %>%
  summarize(
    mean_temp = mean(ground_temp),
    stand_dev_temp = sd(ground_temp),
    max_temp = max(ground_temp),
    min_temp = min(ground_temp),
    temp_range = max_temp - min_temp, # The overall temp range the sites experience
    count = n()
  ) %>%
  ungroup() %>%
  mutate(plot_treatment = substr(plot, 1, 3))


#######################
### Hobo interval data
##################################################
daily_summary <- hobo_data %>%
  group_by(plot, date) %>%
  summarize(
    avg_daily_temperature = mean(ground_temp),
    avg_daily_max_temperature = max(ground_temp),
    daily_temperature_sd = sd(ground_temp)
  ) %>%
  ungroup()

daily_summary <- daily_summary %>%
  mutate(inter = sapply(date, function(d) {
    matching_interval <- interval_data$start_date <= d & d < (interval_data$start_date + interval_data$interval_length)
    if (any(matching_interval)) {
      interval_data$inter[matching_interval][1]
    } else {
      NA
    }
  }))

interval_summary_test <- daily_summary %>%
  group_by(plot, inter) %>%
  summarize(
    avg_interval_temperature = mean(avg_daily_temperature),
    avg_interval_max_temperature = mean(avg_daily_max_temperature),
    interval_temperature_sd = sd(avg_daily_temperature)
  ) %>%
  ungroup() %>%
  mutate()

# view(interval_summary_test)

# interval_summary <- daily_summary %>%
#   left_join(interval_data %>% select(inter, interval_length, plot), by = "plot") %>%
#   mutate(interval_start = date,
#          interval_end = interval_start + interval_length) %>%
#   group_by(plot, interval_start, interval_end) %>%
#   summarize(
#     avg_interval_temperature = mean(avg_daily_temperature),
#     avg_interval_max_temperature = mean(avg_daily_max_temperature),
#     interval_temperature_sd = sd(avg_daily_temperature)
#   ) %>%
#   ungroup()
# 
# view(interval_summary)

#merged_df <- merge(daily_summary, interval_data, by = c("plot", "date"))

