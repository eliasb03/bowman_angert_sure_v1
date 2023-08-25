##################################################
# HOBO Data Loading and Tidying Script
##################################################
# Script: 02_plot-data_v1.R
# Purpose: This code is used to import HOBO data sheets, and plot attributes, into the R environment, for Elias' Lupine SURE 2023, 
#   regarding heat shock, and lupine flower mortality. This code imports the data from CSVs downloaded from 14 HOBO
#   data loggers left in each of the 14 plots in the study.
# Author: Elias Bowman
# Date: 2023-08-03
# Version: 0.0
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


# Data otc and con are now lists that hold 7 large csvs each


# # Create additional rows for each dataframe that include a treatment indicator and a plot name holder
# otc_data <- map(otc_data, ~ .x %>%
#                   mutate(treatment = "OTC", plot = names(.x))
#                 )
 otc_data_mod <- map(names(otc_data), ~ otc_data[[.x]] %>%
                   mutate(treatment = "OTC", plot = .x)
 )

# otc_data_mod <- map(names(otc_data), function(df_name) {
#   otc_data[[df_name]] %>%
#     mutate(treatment = "OTC", plot = df_name)
# })
# 
#  con_dat_mod1 <- map(con_data, ~ .x %>%
#                    mutate(treatment = "CON", plot = names(.x))
#  )

 con_data_mod <- map(names(con_data), ~ con_data[[.x]] %>%
                   mutate(treatment = "CON", plot = .x)
 )

# Combine the modified dataframes into a single list
combined_data <- c(otc_data_mod, con_data_mod)

# Bind all dataframes together
total_hobo <- bind_rows(combined_data)


# 
# # Create a function to add columns and return the modified data frame
# add_columns_to_dataframe <- function(data_frame, treatment, plot) {
#   data_frame$treatment <- as.factor(treatment)
#   data_frame$plot <- as.character(plot)
#   return(data_frame)
# }
# 
# 
# 
# 
# # 
# # List of treatments and corresponding directory paths
# treatments <- c("CON", "OTC")
# ##directories <- c("C:/path_to_CON_directory/", "C:/path_to_OTC_directory/")
# 
# HOBO_data_as_list <- list()
# 
# # # 
# # for (treatment in treatments) {
# #   csv_files <- list.files(path = "C:/Users/elias/OneDrive/Documents/Angert SURE 2023/Bowman_SURE_2023/data/raw/hobo", 
# #                           pattern = paste0(treatment, "-.*\\.csv$"), full.names = TRUE)
# #   
# #   for (i in seq_along(csv_files)) {
# #     csv_file_name <- basename(csv_files[i])  # Get the CSV file name
# #     modified_df <- add_columns_to_dataframe(read_csv(csv_files[i]), treatment, csv_file_name)
# #     HOBO_data_as_list[[length(HOBO_data_as_list) + 1]] <- modified_df
# #   }
# # }
# 
# # Iterate through each data frame in con_data and add columns
# # for (i in seq_along(con_data)) {
# #   csv_file_name <- basename(con_files[i])  # Get the CSV file name
# #   treatment <- ifelse(grepl("CON", csv_file_name), "CON", "OTC")
# #   
# #   # Append the "plot" and "treatment" columns
# #   modified_df <- add_columns_to_dataframe(con_data[[i]], treatment, csv_file_name)
# #   
# #   # Add the modified data frame to the list
# #   HOBO_data_as_list[[i]] <- modified_df
# # }
# # 
# # for (i in seq_along(otc_data)) {
# #   csv_file_name <- basename(otc_files[i])  # Get the CSV file name
# #   treatment <- ifelse(grepl("CON", csv_file_name), "CON", "OTC")
# #   
# #   # Append the "plot" and "treatment" columns
# #   modified_df <- add_columns_to_dataframe(otc_data[[i]], treatment, csv_file_name)
# #   
# #   # Add the modified data frame to the list
# #   modified_otc_data[[i]] <- modified_df
# # }