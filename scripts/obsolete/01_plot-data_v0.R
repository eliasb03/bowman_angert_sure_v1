## 
## Script used to import and clean data acquired from sampling trips in the summer of 2023 by Elias Bowman
## 2023-07-26
##
##
##

# # Import trip data into individual data frames
## Import trip data into individual data frames
# Get a list of file paths for all trip data files
trip_files <- list.files("data/raw", pattern = "Bowman_SURE2023_trip\\d+_v0.csv", full.names = TRUE)

# Create a list to store the trip data frames
trip_data_list <- lapply(trip_files, read.csv)

# Assign order_date based on the number of trips
order_date <- 1:length(trip_data_list)

# Combine the trip data frames into a single data frame using do.call(rbind, ...)
mega_data_raw <- do.call(rbind, trip_data_list)

# Create an index of the trip dates
trip_dates <- data.frame(unique(mega_data_raw$date), order_date = order_date)
colnames(trip_dates) <- c("date", "order")

# trip0_raw <- read.csv("data/raw/Bowman_SURE2023_trip0_v0.csv")
# trip1_raw <- read.csv("data/raw/Bowman_SURE2023_trip1_v0.csv")
# trip2_raw <- read.csv("data/raw/Bowman_SURE2023_trip2_v0.csv")
# trip3_raw <- read.csv("data/raw/Bowman_SURE2023_trip3_v0.csv")
# trip4_raw <- read.csv("data/raw/Bowman_SURE2023_trip4_v0.csv")
# trip5_raw <- read.csv("data/raw/Bowman_SURE2023_trip5_v0.csv")
# trip6_raw <- read.csv("data/raw/Bowman_SURE2023_trip6_v0.csv")
# 
# 
# 
# # Compile all the data into one dataframe
# mega_data_raw <- rbind(trip0_raw, trip1_raw, trip2_raw, trip3_raw, trip4_raw, trip5_raw, trip6_raw)
# 
# 
# # Create an index of the trip dates
# trip_dates <- data.frame(unique(mega_data_raw$date), order_date = c(1,2,3,4,5,6, 7))
# colnames(trip_dates) <- c("date","order")
# 
# tibble(trip_dates)

# Create a function that processes the data types for each column of a standard data frame
process_data_frame <- function(df) {
  df <- df[complete.cases(df[, 4]), ]   # Remove instances of NA, where NA is present in the 4th column - to remove fully na columns
  
  df$date <- parse_factor(df$date, levels = trip_dates$date)   # Column 1: Convert to factor 
  df[, 2] <- as.character(df[, 2])   # Column 2: Convert to character (chr)
  df[, 3] <- as.integer(df[, 3])     # Column 3: Convert to integer (int)
  df[, 4] <- as.integer(df[, 4])     # Column 4: Convert to integer (int)
  df[, 5] <- as.numeric(df[, 5])     # Column 5: Convert to double (dbl)
  df[, 6] <- as.integer(df[, 6])     # Column 6: Convert to integer (int)
  df[, 7] <- as.integer(df[, 7])     # Column 7: Convert to integer (int)
  df[, 8] <- as.integer(df[, 8])     # Column 8: Convert to integer (int)
  df[, 9] <- as.integer(df[, 9])     # Column 9: Convert to integer (int)
  df[, 10] <- as.integer(df[, 10])     # Column 10: Convert to integer (int)
  df[, 11] <- as.character(df[, 11]) # Column 11: Convert to character (chr)
  df[, 13] <- as.character(df[, 13]) # Column 12: Convert to character (chr)
  df[, 14] <- df[, 14] == 'y'        # Column 13: Convert 'y' to TRUE, 'n' to FALSE (lgl)
  df[, 15] <- df[, 15] == 'y' | df[, 15] == '' # Column 14: Convert 'y' to TRUE, blank to FALSE (lgl)
  df[, 16] <- df[, 16] == 'y'        # Column 15: Converts previously finished to true or false (lgl)
  
  # Return the processed data frame
  return(df)
}

# Create a copy of the original data frame with proper formatting
mega_data <- process_data_frame(mega_data_raw)

# Find the greatest date
max_date <- trip_dates[trip_dates$order == max(trip_dates$order), ]$date
max_date_order <- max(trip_dates$order)

# Repeat instances of finished data into following trips
# Any data indicated as "finished" should be repeatedly copied back into the data frame, with updated dates as if it were collected on a new trip
# Additionally, "previously.finished" should be converted to TRUE

# Initialize an empty data frame to store the modified data
updated_data <- data.frame()
prev_finished <- data.frame()

  TEST = 0



# Cycle through mega_data and detect finished rows
for(row_index in 1:nrow(mega_data)) {
  
  # Check if the row is "finished" and not "previously.finished"
  if (mega_data$finished[row_index] & !mega_data$previously.finished[row_index]) {
    # Get the current "date" and find the next factor level based on "trip_dates"
    
    # Calculate the number of times a finished data point should be repeated  
    current_date <- mega_data$date[row_index]
    current_date_order <- which(trip_dates$date == current_date)
    num_repetitions <- max_date_order - current_date_order
    
    # Create a data frame of the now previously finished rows
    for (f in 1:num_repetitions) {
      
    # Find the next date factor level
    # Old Method: next_date_level <- current_date_order + f
    next_date_level <- trip_dates$date[current_date_order + f]
    
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

# 
# # Create the update_data function
# update_data <- function(data) {
#   data %>%
#     mutate(
#       previously.finished = finished & !is.na(date),
#       date = as.factor(lookup_table$next_date[match(as.character(date), lookup_table$date)])
#     )
# }
# 
# # Replace 'YourDataFrame' with the actual name of your large data frame
# mega_data <- mega_data %>%
#   group_by(date) %>%
#   do({
#     updated <- update_data(.)
#     prev_finished <- updated %>%
#       filter(finished) %>%
#       mutate(date = as.factor(lookup_table$next_date[match(as.character(date), lookup_table$date)]))
#     
#     bind_rows(updated, prev_finished)
#     
#   })

# # Function that modifies the data for each level of the "date" factor
# update_data <- function(data, level) {
#   data %>%
#     filter(date == level) %>% 
#     filter(finished) %>%
#     mutate(date = as.factor(lookup_table$next_date[match(as.character(date), lookup_table$date)]),
#            previously.finished = TRUE)
# }
# 
# mega_data <- mega_data %>%
#   group_by(date) %>%
#   do({
#     updated <- update_data(., unique(.$date))
#     prev_finished <<- update_data(updated_data)
#     
#     updated_data <<- bind_rows(updated_data, updated, prev_finished)
#     .
#     view(updated_data)
#   })

# dunno if the thing above this is really working

# Add the modified data back to the original dataframe


##### updated_data <- process_data_frame(updated_data)
##### mega_data <- bind_rows(mega_data, updated_data)


# 
# # Filter the rows where "finished" is true
# selected_rows <- m  ega_data %>% filter(finished)
# 
# # Identify the next date for each row with "finished" as true
# next_dates <- mega_data %>% 
#   filter(date > max(selected_rows$date)) %>%  # Filter dates greater than the maximum date in selected_rows
#   summarise(next_date = min(date))           # Find the minimum date from the filtered dates
# 
# # Create a copy of the selected data with updated date and "previously.finished" set to true
# selected_rows_modified <- selected_rows %>%
#   mutate(date = next_dates$next_date, previously.finished = TRUE)
# 
# # Append the modified data to the original large data frame
# df_copy <- bind_rows(df_copy, selected_rows_modified)

# mega data is all the data from all the trips in one big file
# need to repeat instances of data in which they are finished and and back to the data frame with an updated date
# also update previously finished to true
# selecting for data that is finished, within a range of cells and making a temp dataframe is probably best
# ask chatgpt


# 
# # List of data frame names
# data_frame_names <- c("trip0_raw", "trip1_raw", "trip2_raw", "trip3_raw", "trip4_raw", "trip5_raw") # add in: ,"trip6_raw") when trip 6 is ready
# 
# 
# 
# # Loop through the data frames and process them sequentially
# for (df_name in data_frame_names) {
#   if (exists(df_name)) {
#     # Get the df using the name and apply function to it
#     assign(df_name, process_data_frame(get(df_name)))
#   } else {
#     # Case if the data frame with the given name doesn't exist
#     print(paste("Data frame", df_name, "not found. Skipping..."))
#   }
# }
# 
# 
# ## carrying finished data between trips
# raw_data_frames <- list(trip0_raw, trip1_raw, trip2_raw, trip3_raw, trip4_raw, trip5_raw)
# trip_data <- list()
# 
# 
# # Function to create an empty data frame with the correct column structure as the trip data
# create_empty_df <- function(template_df) {
#   empty_df <- data.frame(matrix(nrow = 0, ncol = ncol(template_df)))
#   colnames(empty_df) <- colnames(template_df)
#   return(empty_df)
# }
# for (i in 0:(length(raw_data_frames) - 1)) {
#   current_df <- raw_data_frames[i]
#   
#   # If it's the first data frame (i = 0), just add it to the trip_data list as is
#   if (i == 0) {
#     trip_data[[i + 1]] <- current_df
#   } else {
#     prev_df <- trip_data[i]
#     
#     # Identify rows with "finished" as TRUE in the previous data frame
#     finished_rows <- prev_df$finished == TRUE
#     
#     # Add finished rows to the current data frame (if any)
#     if (any(finished_rows)) {
#       # Create an empty data frame with the correct column structure
#       current_df_empty <- create_empty_df(current_df)
#       
#       # Add the finished rows from the previous data frame
#       current_df_empty <- rbind(current_df_empty, prev_df[finished_rows, ])
#       
#       # Update "previously.finished" in the current data frame for the added rows
#       current_df_empty$previously.finished <- 
#         with(current_df_empty, ifelse(row.names(current_df_empty) %in% row.names(prev_df[finished_rows, ]), TRUE, previously.finished))
#       
#       # Add the modified current data frame to the trip_data list
#       current_df_empty
#       trip_data[i + 1] <- current_df_empty
#     }
#   }
# }
# trip_data
# 
# # Name the trip_data list elements as "trip0_data", "trip1_data", etc.
# names(trip_data) <- paste0("trip", 0:length(trip_data) - 1, "_data")
# 
# # Now, you have the new data frames "trip0_data", "trip1_data", etc. with the appended data.
# 
# View(trip0_data)


# 
# #selecting for data of finished racemes
# trip0_finished <- filter(trip0_raw, finished == "y")
#   trip0_finished$mature <- as.integer(trip0_finished$mature)
#   trip0_finished$raceme_length <- as.numeric(trip0_finished$raceme_length)
# 
# trip1_finished <- filter(trip1_raw, finished == "y")
# 
# trip2_finished <- filter(trip2_raw, finished == "y")
#   trip2_finished$raceme_length <- as.numeric(trip2_finished$raceme_length)
#   trip2_finished$budding <- as.integer(trip2_finished$budding)
#   trip2_finished$mature <- as.integer(trip2_finished$mature)
#   trip2_finished$post_flowering <- as.integer(trip2_finished$post_flowering)
#   trip2_finished$missing <- as.integer(trip2_finished$missing)
#   trip2_finished$fruiting <- as.integer(trip2_finished$fruiting)
#   
#   
# tibble(trip2_finished)
# trip3_finished <- filter(trip3_raw, finished == "y")
# trip4_finished <- filter(trip4_raw, finished == "y")
# trip5_finished <- filter(trip5_raw, finished == "y")
# 
# # appending finished racemes onto later trips
# ## need to edit the data, such that previously finished, now = y
# ## need to append the data to the next data frame
# ## need to update dates to match current observations
# # setting up trip 0 data
# trip0_data <- trip0_raw
# 
# #setting up trip 1 data
# rolling_finish <- mutate(trip0_finished[, -15], previously.finished = "y")
# trip1_data <- bind_rows(trip1_raw, rolling_finish)
# 
# #setting up trip 2 data
# rolling_finish <- bind_rows(rolling_finish, mutate(trip1_finished[, -15], previously.finished = "y"))
# trip2_data <- bind_rows(trip2_raw, rolling_finish)
# 
# #setting up trip 3 data
# rolling_finish <- bind_rows(rolling_finish, mutate(trip2_finished[, -15], previously.finished = "y"))
# trip3_data <- bind_rows(trip3_raw, rolling_finish)
# 
# #setting up trip 4 data
# rolling_finish <- bind_rows(rolling_finish, mutate(trip3_finished[, -15], previously.finished = "y"))
# trip4_data <- bind_rows(trip4_raw, rolling_finish)
# 
# #setting up trip 5 data
# rolling_finish <- bind_rows(rolling_finish, mutate(trip4_finished[, -15], previously.finished = "y"))
# trip5_data <- bind_rows(trip5_raw, rolling_finish)
# 
# ## NEED TO CHECK IF THE ABOVE ACTUALLY WORKS OR MAKES SENSE, AND THEN I NEED TO EDIT THE DATES
# 
# 
# 
