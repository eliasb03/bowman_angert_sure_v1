##################################################
# Plot Level Linear Model
##################################################
# Script: 07_linear-model_v0.R
# Purpose: This code is going to create a linear model relating heat stress, as plot average daily max temp,
#   and plot level average of missing flowers
# Author: Elias Bowman
# Date: 2023-08-14
# Version: 0.0
##################################################

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

# heat.stress_model <- 

plot_level_data
plant_level_data
