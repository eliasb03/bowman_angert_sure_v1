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

## OLD ATTEMPTS \/
#######################
# view(hb.sum_by.plot)
# hb.sum_by.plot provides data of: plot --> mean_daily_max
# in order to pair with this i need: plot --> number of missing
# but i run into a technical issue of what that means because missing flowers is 
# a) variable dependent on raceme 
# - we could start by taking an average number for a plot, but i think a metric based on a proportion of total flowers makes more sense
# - maybe a percent missing: (number missing)/(sum(bud, mature, missing, post, fruit))
#   - take this for each raceme, average it to the plant level, then plot level
#   - alt: take this and find the change in the proportion missing from the start to the end of a plant
# b) temporal
# - because I have heat stress over the entire experiment, I shuld get a single value over the whole experiment
# - unless I want to change my heat stress metric to a temporally variable one
# 
# # Currently the trip data im using for this model is essentially gibbrish
# 
# ### Join the two relevant tables
# modelling.table <- inner_join(
#   plot.no.date_data,
#   hb.sum_by.plot,
#   by = "plot")
# 
# ### Assessing Linearity
# # Scatterplot and cor.test()
# coefficient <- cor.test(modelling.table$prop_missing.p, modelling.table$mean_daily_max)
# # Display linearity summary
# coefficient
# 
# ### Create a linear regression model
# heat.stress_model <- lm(prop_missing.p ~ mean_daily_max, data = modelling.table)
# # Display the model summary
# summary(heat.stress_model)
# 
# ### Linear Regression Plot
# linear.plot <- ggplot(modelling.table, aes(x = mean_daily_max, y = prop_missing.p)) + 
#   geom_point() +
#   stat_smooth(method = "lm", col = "black") +
#   geom_smooth(se = FALSE, color = "red")
# # Display the regression plot
# linear.plot
# 
# 
# ### Second linear model concept
# this one is gonna try to use interval data
# Currently my interval plant data has errors in that it has discrepancies in terms of the actually number of flowers counted