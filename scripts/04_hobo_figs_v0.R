##################################################
# HOBO Data Initial Figures
##################################################
# Script: 04_hobo_figs_v0.R
# Purpose: This code is used to display HOBO data, preliminary figures such as displaying plot temperature
#   as a result of treatment groupings
# Author: Elias Bowman
# Date: 2023-08-14
# Version: 0.0
##################################################

### Plots displaying Max Temp based on treatment ###
## Violin plot
max.temp_violin <- hb.sum_by.day %>% 
  ggplot(aes(x = plot_treatment, y = max_temp)) +
  geom_violin() +
  geom_jitter(shape = 1, size = .5, width = .3) +
  xlab("Treatment") +
  ylab("Maximum Temperature (C)") +
  ylim(0, 60) +
  theme_bw()
## Box plot
max.temp_boxplot <- hb.sum_by.day %>% 
  ggplot(aes(x = plot_treatment, y = max_temp)) +
  geom_boxplot() +
  geom_jitter(shape = 1, size = .5, width = .3) +
  xlab("Treatment") +
  ylab("Maximum Temperature (C)") +
  ylim(0, 60) +
  theme_bw()
## Combined Box Violion Plot
max.temp_bv.plot <- hb.sum_by.day %>% 
  ggplot(aes(x = plot_treatment, y = max_temp)) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  #  geom_jitter(shape = 1, size = .5, width = .3) +
  xlab("Treatment") +
  ylab("Maximum Temperature (C)") +
  ylim(0, 60) +
  theme_bw()

max.temp_boxplot 
#max.temp_violin
max.temp_bv.plot

### Plots displaying ###
