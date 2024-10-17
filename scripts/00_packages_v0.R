##################################################
# Package Loading Script
##################################################
# Script: 00_packages_v0.R
# Purpose: This code is used to import relevant packages into the R environment, 
#              for Elias' Lupine SURE 2023, regarding heat shock, and flower mortality
# Author: Elias Bowman
# Date: 2023-08-03
# Version: 0.0
##################################################

## Set script working directory
setwd("C:/Users/elias/OneDrive/Documents/Angert SURE 2023/Bowman_SURE_2023")

## Set script wide variables
current_year <- as.integer(format(Sys.Date(), "%Y"))

## Import Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(devtools)
library(skimr)
library(readr)
library(purrr)
library(knitr)
library(naniar)
library(janitor)
library(broom)
library(car)
library(lubridate)
library(lme4)

