##################################################
# HOBO Data Initial Analysis
##################################################
# Script: 05_hobo_assess_v0.R
# Purpose: This code is used to start analyzing HOBO data, seeing if the temperature means, mean temp maxs,
#   and other variables are signifcantly different between treatment groups
# Author: Elias Bowman
# Date: 2023-08-14
# Version: 0.0
##################################################

## Creating a summary statistics table regarding the max temp data sorted on treatment
# Includes 95 percent confidence intervals regarding a 2 sample t test
max_temp.stats <- hb.sum_by.day %>%
  group_by(plot_treatment) %>%
  summarise(
    Count = n() - naniar::n_miss(max_temp),
    Count_NA = naniar::n_miss(max_temp), 
    Mean = mean(max_temp, na.rm = TRUE),
    SD = sd(max_temp, na.rm = TRUE),
    SEM = SD/sqrt(Count),
    Low_95_CL = t.test(max_temp, conf.level = 0.95)$conf.int[1],
    Up_95_CL = t.test(max_temp, conf.level = 0.95)$conf.int[2]
  )

## Table to display
# kable(max_temp.stats, digits = 4)

## Assumptions of the two sample t-test
# Test for normality
max_temp.normality <- hb.sum_by.day %>%
  ggplot(aes(sample = max_temp)) +
  stat_qq(shape = 1, size = 2) +
  stat_qq_line() +
  facet_grid(~ plot_treatment) +
  xlab("Normal quantile") +
  ylab("Max Temperature (C)") +
  theme_bw()

### I'M NOT THE MOST CONFIDENT AT USING NORMAL QUANTILE PLOTS BUT THIS DOESNT SEEM IDEAL
# Shapiro Test for Normality 
# used to backup normal quantile
# Had trouble executing a shapiro test on data grouped by treatment
# max_temp.shapiro <- hb.sum_by.day %>% 
#   group_by(plot_treatment) %>%
#   shapiro.test(.$max_temp)
# 
# hb.sum_by.day %>% 
#   group_by(plot_treatment) %>%
#   summarise(results = data.frame(shapiro.test(.$max_temp)))
# 
# shapiro.result1 <- hb.sum_by.day %>%
#   filter(plot_treatment == "OTC") %>%
#   shapiro.test(.$mean_temp)
# shapiro.result2 <- hb.sum_by.day %>%
#     filter(plot_treatment == "CON") %>%
#     shapiro.test(.$mean_temp)
# 
# subset_data <- hb.sum_by.day[hb.sum_by.day$plot_treatment == "CON", "mean_temp"]
# subset_data <- as.numeric(subset_data)
# shapiro_result <- shapiro.test(subset_data)


# Test for equal variance among groups
max_temp.vartest <- leveneTest(max_temp ~ plot_treatment, data = hb.sum_by.day)
