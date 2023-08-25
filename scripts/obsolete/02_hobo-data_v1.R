##################################################
# HOBO Data Loading and Tidying Script
##################################################
# Script: 02_plot-data_v1.R
# Purpose: This code is used to import HOBO data sheets, and plot attributes, into the R environment, for Elias' Lupine SURE 2023, 
#   regarding heat shock, and lupine flower mortality. This code imports the data from CSVs downloaded from 14 HOBO
#   data loggers left in each of the 14 plots in the study.
# Author: Elias Bowman
# Date: 2023-08-03
# Version: 1.0
##################################################

## Import Hobo data
# Establish CSV path
csv_path <- "C:/Users/elias/OneDrive/Documents/Angert SURE 2023/Bowman_SURE_2023/data/raw/hobo"

# Get a list of file paths for all CON and OTC data files
con_files <- list.files(path = csv_path, pattern = "CON-.*\\.csv$", full.names = TRUE)
con_file_names <- basename(con_files)
otc_files <- list.files(path = csv_path, pattern = "OTC-.*\\.csv$", full.names = TRUE)
otc_file_names <- basename(otc_files)


# Create a function to import data handling errors with a tryCatch
read_csv_error <- function(file_path){
  tryCatch(
    {
      read.csv(file_path, skip = 1)
    },
    error = function(e) {
      return(paste("Error reading file:", file_path, "Error message:", e$message))
    }
  )
}


# Read in CSV files and use error handling function
con_data <- lapply(con_files, read_csv_error)
names(con_data) <- gsub("\\.csv$", "", con_file_names)
otc_data <- lapply(otc_files, read_csv_error)
names(otc_data) <- gsub("\\.csv$", "", otc_file_names)

# 
# # Create additional rows for each dataframe, remove unused ones, and rename HOBO columns
# # Include a treatment indicator and a plot name holder
# 
# otc_data_mod <- map(names(otc_data), ~ otc_data[[.x]] %>%
#                       select(1:3) %>% # Selecting for only, index, date, and temperature columns
#                       rename(obs = !!names(.)[1]) %>%
#                       rename(date.time = !!names(.)[2]) %>%
#                       rename(ground_temp = !!names(.)[3]) %>%
#                       mutate(treatment = "OTC", plot = .x))
# names(otc_data_mod) <- gsub("\\.csv$", "", otc_file_names)
# 
# con_data_mod <- map(names(con_data), ~ con_data[[.x]] %>%
#                       select(1:3) %>% # Selecting for only, index, date, and temperature columns
#                       rename(obs = !!names(.)[1]) %>%
#                       rename(date.time = !!names(.)[2]) %>%
#                       rename(ground_temp = !!names(.)[3]) %>%
#                       mutate(treatment = "CON", plot = .x))
# names(con_data_mod) <- gsub("\\.csv$", "", con_file_names)
# 

process_data <- function(data_list, file_names, treatment_name) {
  modified_data <- map(names(data_list), ~ data_list[[.x]] %>%
                         select(1:3) %>%
                         rename(obs = !!names(.)[1],
                                date.time = !!names(.)[2],
                                ground_temp = !!names(.)[3]) %>%
                         mutate(treatment = treatment_name, plot = .x))
  
  names(modified_data) <- gsub("\\.csv$", "", file_names)
  return(modified_data)
}

otc_data_mod <- process_data(otc_data, otc_file_names, "OTC")
con_data_mod <- process_data(con_data, con_file_names, "CON")


# Combine the modified dataframes into a single list
combined_data <- c(otc_data_mod, con_data_mod)

# Bind all dataframes together
total_hobo <- bind_rows(combined_data)
total_hobo <- total_hobo[, c(5, 1:4)]
