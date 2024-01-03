##################################################
# HOBO Data Loading and Tidying Script
##################################################
# Script: 02_plot-data_v1.R
# Purpose: This code is used to import HOBO data sheets, and plot attributes, into the R environment, for Elias' Lupine SURE 2023, 
#   regarding heat shock, and lupine flower mortality. This code imports the data from CSVs downloaded from 14 HOBO
#   data loggers left in each of the 14 plots in the study.
# Author: Elias Bowman
# Date: 2023-08-15
# Version: 2.0
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

 
# Create additional rows for each dataframe, remove unused ones, and rename HOBO columns
# Include a treatment indicator and a plot name holder

# Create a function to process HOBO data
clean_hobo_data <- function(data_list, file_names, treatment_name) {
  modified_data <- map(names(data_list), ~ data_list[[.x]] %>%
                         select(1:3) %>%
                         rename(obs = !!names(.)[1],
                                date.time = !!names(.)[2],
                                ground_temp = !!names(.)[3]) %>%
                         mutate(treatment = treatment_name, plot = .x))
  
  names(modified_data) <- gsub("\\.csv$", "", file_names)
  return(modified_data)
}
otc_data_mod <- clean_hobo_data(otc_data, otc_file_names, "OTC")
con_data_mod <- clean_hobo_data(con_data, con_file_names, "CON")


# Combine the modified dataframes into a single list
combined_data <- c(otc_data_mod, con_data_mod)

# Bind all dataframes together
total_hobo <- bind_rows(combined_data)

# Convert date time into proper date formatting
total_hobo <- total_hobo %>%
  mutate(date.time = as.POSIXct(strptime(date.time, format = "%m/%d/%y %I:%M:%S %p")),
         date = as.Date(date.time, format = "%Y-%m-%d %H:%M:%S"),
         time = format(date.time, format = "%H:%M:%S"))



## Import Plot Level Specific Data
plot_details <- read.csv("C:/Users/elias/OneDrive/Documents/Angert SURE 2023/Bowman_SURE_2023/data/raw/plot_specifics.csv")
# add columns regarding plot averages and format HOBO cutoff dates
plot_details <- plot_details %>%
  mutate(soil_moisture = (soil_1 + soil_2)/2) %>%
  mutate(canopy_cover = (canopy_1 + canopy_2)/2) %>%
  mutate(cutoff = as.POSIXct(strptime(cutoff, format = "%Y-%m-%d %H:%M")))

## Process Hobo Data
# create dataframe to store processed hobo data
hobo_data <- data.frame()

# Select for each plot 1 by 1, and remove any observations that are after the cut off date 
# All data before the respective cut off dates can be returned and stored in hobo_data
plots <- unique(total_hobo$plot)
for(x in plots){
  cutoff_time <- filter(plot_details, plot == x)$cutoff
  temp_hobo <- total_hobo[total_hobo$plot == x,]
  temp_hobo <- temp_hobo %>% filter(date.time < cutoff_time)
  hobo_data <- rbind(hobo_data, temp_hobo)
}

# Changing formating of "plot" in hobo_data to be congruent with other cases
# "CON-1" --> "CON1"
hobo_data$plot <- gsub("-", "", hobo_data$plot)

# "hobo_data" is the consolidated and organized datasheet for hobo data
# only current issue is the extreme temperature spikes

write_csv(hobo_data, "C:/Users/elias/OneDrive/Documents/Angert SURE 2023/Bowman_SURE_2023/data/processed/hobo_data.csv")
# 
# OTC_plot <- hobo_data %>%
#   filter(plot == "OTC5") %>%
#   ggplot(aes(x = date.time, y = ground_temp)) +  # Set x and y aesthetics
#   geom_line() +                       # Create a line plot
#   theme_minimal()
# OTC_plot
