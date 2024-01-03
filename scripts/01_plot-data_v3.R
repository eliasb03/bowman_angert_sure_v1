##################################################
# Plot Data Loading and Tidying Script
##################################################
# Script: 01_plot-data_v3.R
# Purpose: This code is used to import plot data sheets into the R environment, for Elias' Lupine SURE 2023, 
#   regarding heat shock, and lupine flower mortality. This code imports the data from CSVs, and compiles it all
#   into a single data frame, which is then tidied. Further, it repeats observations of "Finished" racemes that were 
#   noted to reduce sampling. This leads to a fully organized and completed data table "trip_data" with all observations
# Author: Elias Bowman
# Date: 2023-08-30
# Version: 3.0
##################################################

## Set script wide variables
current_year <- as.integer(format(Sys.Date(), "%Y"))

##################################################
### Import trip data
##################################################
# Import Data
# Get a list of file paths for all trip data files
trip_files <- list.files("data/raw/trips", pattern = "Bowman_SURE2023_trip\\d+_v0.csv", full.names = TRUE)

# Import trip data with error handling using tryCatch
trip_data_list <- lapply(trip_files, function(file_path) {
  tryCatch(
    {
      read.csv(file_path)
    },
    error = function(e) {
      message(paste("Error reading file:", file_path))
      NULL
    }
  )
})

# Remove NULL elements from the list (failed reads)
trip_data_list <- trip_data_list[!sapply(trip_data_list, is.null)]

# Assign order_date based on the number of trips
order_date <- 1:length(trip_data_list)

# Combine the trip data frames into a single data frame
mega_data_raw <- do.call(rbind, trip_data_list)

# Create an index of the trip dates
trip_dates <- data.frame(unique(mega_data_raw$date), order_date = order_date)
colnames(trip_dates) <- c("date", "order")
final_trip_date <- tail(trip_dates$date, n=1)

##################################################
## Tidying the Data
##################################################
# Create a function that processes the data types for each column of a standard data frame
process_data_frame <- function(df) {
  df <- df[complete.cases(df[, 4]), ]# Remove instances of NA, where NA is present in the 4th column - to remove fully na columns
  # In practice on my data set, this removes a couple instances where I noted a plants presence but took no data - it is good they are removed
  
  df$date <- factor(df$date, levels = trip_dates$date)   # Column 1: Convert to factor 
  df[, 2] <- as.character(df[, 2])   # Column 2: Convert to character (chr)
  df[, 3] <- as.integer(df[, 3])     # Column 3: Convert to integer (int)
  df[, 4] <- as.integer(df[, 4])     # Column 4: Convert to integer (int)
  df[, 5] <- as.numeric(df[, 5])     # Column 5: Convert to double (dbl)
  df[, 6] <- as.integer(df[, 6])     # Column 6: Convert to integer (int)
  df[, 7] <- as.integer(df[, 7])     # Column 7: Convert to integer (int)
  df[, 8] <- as.integer(df[, 8])     # Column 8: Convert to integer (int)
  df[, 9] <- as.integer(df[, 9])     # Column 9: Convert to integer (int)
  df[, 10] <- as.integer(df[, 10])   # Column 10: Convert to integer (int)
  df[, 11] <- as.character(case_when( # Column 11: Convert to character and fills in Column 11 with codes based on Column 12 (chr)
        grepl("abr|aborted|aborted raceme|whole raceme aborted|fully aborted raceme|fully aborted", df[, 12], ignore.case = TRUE) ~ "abr",
        grepl("abt|aborted tip|aborted top|aborted end", df[, 12], ignore.case = TRUE) ~ "abt",
        grepl("bud|budding stage|budding|bud|budding phase|budding raceme", df[, 12], ignore.case = TRUE) ~ "bud",
        TRUE ~ "none" ))
  df[, 12] <- as.character(df[, 12]) # Column 12: Convert to character (chr)
  df[, 13] <- as.character(df[, 13]) # Column 13: Convert to character (chr)
  df[, 14] <- df[, 14] == 'y'        # Column 14: Convert 'y' to TRUE, 'n' to FALSE (lgl)
  df[, 15] <- ifelse(df[, 15] == 'y', TRUE, FALSE) # Column 15: Convert 'y' to TRUE, blank to FALSE (lgl)
  df[, 16] <- df[, 16] == 'y'        # Column 16: Converts previously finished to true or false (lgl)
  
  # Return the processed data frame
  return(df)
}


## Importing the plot characteristics data into "plot_chars"
tryCatch(
  {
    plot_chars <- read.csv("C:/Users/elias/OneDrive/Documents/Angert SURE 2023/Bowman_SURE_2023/data/raw/trips/Bowman_SURE2023_plot-chars_v0.csv")
  },
error = function(e) {
  message(paste("Error reading plot chars file"))
  NULL
})

# Convert "standard_plants" into separate plant IDs, held in lists
plot_chars$standard_plants <- as.character(plot_chars$standard_plants)
plot_chars$standard_plants <- lapply(strsplit(plot_chars$standard_plants, ", "), as.numeric)

# Create a copy of the original data frame with proper formatting
mega_data <- process_data_frame(mega_data_raw)

## Filtering and selecting mega_data only for plants that were included in the experiment
# Based off what was held in plot_chars "standard_plants"
mega_data <- mega_data %>%
  left_join(plot_chars, by = "plot") %>%
  mutate(matches_standard = map2_lgl(plant_id, standard_plants, ~ .x %in% .y)) %>%
  filter(matches_standard) %>%
  select(-matches_standard) %>%
  select(1:16) # Selects for only the first 16 columns, removes the plot_chars data
colnames(mega_data)[1] ="date" # rename date column back from "date.x" to "date"

##################################################
## Repeat instances of finished data into following trips
##################################################
# Any data indicated as "finished" should be repeatedly copied back into the data frame, with updated dates as if it were collected on a new trip
# Additionally, "previously.finished" should be converted to TRUE

# Initialize an empty data frame to store the modified data
updated_data <- data.frame()

# Cycle through mega_data and detect finished rows
for(row_index in 1:nrow(mega_data)) {
  
  # Check if the row is "finished" and not "previously.finished"
  if (mega_data$finished[row_index] & !mega_data$previously.finished[row_index] 
      & mega_data$date[row_index]!=final_trip_date) {
    # Get the current "date" and find the next factor level based on "trip_dates"
    
    # Calculate the number of times a finished data point should be repeated
    current_date <- mega_data$date[row_index]
    current_date_order <- which(trip_dates$date == current_date)
    num_repetitions <- (max(trip_dates$order)) - current_date_order
    
    # Create a data frame of the now previously finished rows
    for (date_repeats in 1:num_repetitions) {
      
      # Find the next date factor level
      next_date_level <- trip_dates$date[current_date_order + date_repeats]
      
      # Create a new row with the updated information
      new_row <- mega_data[row_index,]
      new_row$date = next_date_level
      new_row$finished = TRUE
      new_row$previously.finished = TRUE
      
      # Add the new row as many times as the remaining factor levels
      updated_data <- rbind(updated_data, new_row)
    }
  }
}

trip_data <- rbind(mega_data, updated_data)
# "trip_data" is the final collection of all data

##################################################
# Create a function that creates  the phenological index of a given piece of data
calculate_phenological_index <- function(budding_sum, mature_sum, post_flowering_sum, fruiting_sum) {
  numerator <- 1 * budding_sum + 2 * mature_sum + 3 * post_flowering_sum + 3 * fruiting_sum
  denominator <- sum(budding_sum, mature_sum, post_flowering_sum, fruiting_sum)
  
  if (denominator == 0) {
    ph_index <- 0
  } else {
    ph_index <- numerator / denominator
  }
  
  return(ph_index)
}

##################################################
## Create a dataframe especially for pollinated plant data
pollinated_data <- trip_data %>%
  filter(pollination_plant == "y")
# "pollinated_data" is a dataframe of only data relevant to pollination

##################################################
## Create data frame averaged to the plant level
# This is with na.rm = TRUE, not entirely sure if my approach should be this simple
plant_data <- trip_data %>%
  filter(pollination_plant == FALSE) %>%
  group_by(date, plot, plant_id) %>%
  summarize(
    num_racemes = n_distinct(raceme_id),
    raceme_length_avg = mean(raceme_length, na.rm = TRUE),
    budding_sum = sum(budding, na.rm = TRUE),
    mature_sum = sum(mature, na.rm = TRUE),
    post_flowering_sum = sum(post_flowering, na.rm = TRUE),
    missing_sum = sum(missing, na.rm = TRUE),
    fruiting_sum = sum(fruiting, na.rm = TRUE),
    prop_missing = (missing_sum / (budding_sum + mature_sum + missing_sum + post_flowering_sum + fruiting_sum)),
    code_list = list(code)
  ) %>%
  mutate(ph_index = calculate_phenological_index(budding_sum, mature_sum, post_flowering_sum, fruiting_sum)) %>%
  ungroup()

## Create a dataframe averaged to the plot level
# Plot observations still seperated by dates
plot.date_data <- plant_data %>%
  group_by(date, plot) %>%
  summarize(
    num_plants = n_distinct(plant_id),
    num_racemes = sum(num_racemes),
    raceme_length_avg.p = mean(raceme_length_avg, na.rm = TRUE),
    budding_sum.p = sum(budding_sum, na.rm = TRUE),
    mature_sum.p = sum(mature_sum, na.rm = TRUE),
    post_flowering_sum.p = sum(post_flowering_sum, na.rm = TRUE),
    missing_sum.p = sum(missing_sum, na.rm = TRUE),
    fruiting_sum.p = sum(fruiting_sum, na.rm = TRUE),
    prop_missing.p = (missing_sum.p / (budding_sum.p + mature_sum.p + missing_sum.p + post_flowering_sum.p + fruiting_sum.p)),
    code_list.p = list(code_list)
  ) %>%
  ungroup()

# Writing my data back into my computer to be saved
write_csv(trip_data, "C:/Users/elias/OneDrive/Documents/Angert SURE 2023/Bowman_SURE_2023/data/processed/trip_data.csv")

##################################################
## Creating an interval based data
# Create a new dataframe for intervals and dates
date_intervals <- unique(plant_data$date) %>%
  as.Date(paste(current_year, date_intervals, sep = "-"), format = "%Y-%d-%b")

# Calculate interval information
interval_info <- data.frame(
  interval_number = 1:(length(date_intervals) - 1),
  interval_start = date_intervals[-length(date_intervals)],
  interval_end = date_intervals[-1]) %>%
  mutate(interval_length = as.numeric(difftime(interval_end, interval_start, units = "days")))

## Creating the interval dataframe
interval_convert <- plant_data[, names(plant_data) != "prop_missing"]
interval_convert$flower_count <- rowSums(interval_convert[, grep("sum$", colnames(interval_convert))])

# Step 1: Convert 'date' factor to a proper date format
interval_convert$date <- as.Date(paste(current_year, interval_convert$date, sep = "-"), format = "%Y-%d-%B")

# Step 2: Sort the dataframe by 'plot', 'plant_id', and 'date'
interval_convert <- interval_convert %>%
  arrange(plot, plant_id, date)

# Step 3: Calculate changes over intervals and create the new dataframe
interval_data <- interval_convert %>%
  group_by(plot, plant_id) %>%
  mutate(interval_length = difftime(lead(date), date, units = "days"),
         interval_number = row_number(),
         inter = paste0("inter", interval_number),
         start_date = date,
         end_date = lead(date),
         interval_budding_sum = lead(budding_sum) - budding_sum,
         interval_mature_sum = lead(mature_sum) - mature_sum,
         interval_post_flowering_sum = lead(post_flowering_sum) - post_flowering_sum,
         interval_missing_sum = lead(missing_sum) - missing_sum,
         interval_fruiting_sum = lead(fruiting_sum) - fruiting_sum,
         flower_count1 = flower_count,
         flower_count2 = lead(flower_count)) %>%
  ungroup()

#Step 4: Select relevant columns and remove rows with NAs (last interval)
interval_data <- interval_data %>%
 select(inter, interval_length, plot, plant_id, start_date, end_date, starts_with("interval"), flower_count1, flower_count2) %>%
 filter(!is.na(interval_budding_sum))

