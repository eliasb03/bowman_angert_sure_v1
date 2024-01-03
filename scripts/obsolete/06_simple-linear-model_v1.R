##################################################
# Simple Linear Model
##################################################
# Script: 06_linear-model_v0.R
# Purpose: This code is going to create a linear model relating heat stress, time above a threshold temperature,
#   and cumulative plant level proportion of missing flowers
# Author: Elias Bowman
# Date: 2023-08-25
# Version: 0.1
##################################################

threshold_temperature <- 35

time_above_threshold <- hobo_data %>%
  group_by(plot) %>%
  summarize(total_time_above_threshold = sum(ifelse(ground_temp > threshold_temperature, 1, 0)))

simple.model_dataframe <- plant_data %>%
  select(date, plot, plant_id, num_racemes, prop_missing) %>%
  filter(date %in% "01-Aug") %>%
  left_join(time_above_threshold, by = "plot")

simple.model_fig <- ggplot(data = simple.model_dataframe) +
  geom_point(aes(x = total_time_above_threshold, y = prop_missing), shape = 1) +
  xlab("Time above temperature threshold") +
  ylab("Proportion Missing") +
  theme_bw() +
  scale_x_continuous()

simple.model <- lm(prop_missing ~ total_time_above_threshold, data = simple.model_dataframe)

simple.linear.plot <- ggplot(simple.model_dataframe, aes(x = total_time_above_threshold, y = prop_missing)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "black") +
  geom_smooth(se = FALSE, color = "red")

