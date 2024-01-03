##################################################
# Cumulative Linear Model
##################################################
# Script: 07_cumulative-linear-model_v1.R
# Purpose: This code is going to create a linear model relating heat stress, time above a threshold temperature,
#   and cumulative plant level proportion of missing flowers
# Author: Elias Bowman
# Date: 2023-08-29
# Version: 2.0
##################################################

## Summarize and collect data into a dataframe for the models
cumulative.model_dataframe <- plant_data %>%
  select(date, plot, plant_id, num_racemes, prop_missing, ph_index) %>%
  filter(date %in% "01-Aug") %>%
  left_join(time_above_threshold, by = "plot") %>%
  left_join(severity_above_threshold, by = "plot") %>%
  mutate(treatment = str_trunc(plot, 3, "right", "")) %>% 
  mutate(plot = as.factor(plot))

###
# s - simple
# c - cumulative

# Creating a figure for the simple cumulative model
sc.model_fig <- ggplot(data = cumulative.model_dataframe) +
  geom_point(aes(x = total_time_above_threshold, y = prop_missing), shape = 1) +
  xlab("Time above temperature threshold") +
  ylab("Proportion Missing") +
  theme_bw() +
  scale_x_continuous()

# Creating the sc model
sc.model <- lmer(prop_missing ~ total_time_above_threshold + ph_index + treatment + (1|plot), data = cumulative.model_dataframe)

# Creating the sc linear model plot
sc.linear.plot <- ggplot(cumulative.model_dataframe, aes(x = total_time_above_threshold, y = prop_missing)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "black") +
  geom_smooth(se = FALSE, color = "red")

###
# i - integral
# c - cumulative
# Creating a figure for the integral cumulative model
ic.model_fig <- ggplot(data = cumulative.model_dataframe) +
  geom_point(aes(x = heat_severity, y = prop_missing), shape = 1) +
  xlab("Time above temperature threshold") +
  ylab("Proportion Missing") +
  theme_bw() +
  scale_x_continuous()

# Creating the ic model
ic.model <- lmer(prop_missing ~ heat_severity + ph_index + treatment + (1|plot), data = cumulative.model_dataframe)

# Creating the ic linear model plot
ic.linear.plot <- ggplot(cumulative.model_dataframe, aes(x = heat_severity, y = prop_missing)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "black") +
  geom_smooth(se = FALSE, color = "red")


create_summary_table <- function(model) {
  # Summarize the model with broom.mixed
  summary_table <- tidy(model, effects = "fixed")
  
  print(summary_table)
}

# print(sc.linear.plot)
# print(ic.linear.plot)

create_summary_table(sc.model)
create_summary_table(ic.model)

