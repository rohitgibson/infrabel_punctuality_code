# This script processes raw railway operational data from Infrabel
# to calculate segment travel durations and stop durations for individual train journeys.

# (1) Load Data
# Load the raw data from a user-selected CSV file.

raw_data_aug = read.csv("~/Downloads/Data_raw_punctuality_202508.csv")
raw_data_sep = read.csv("~/Downloads/Data_raw_punctuality_202509.csv")
raw_data_sep$OP1_COD = NULL

infrabel_raw = rbind(
  raw_data_aug,
  raw_data_sep
)

# Split the main data frame into a list of data frames, one for each unique train number.
# This prepares the data for parallel or sequential processing by train.
infrabel_raw_split = split(infrabel_raw, infrabel_raw$TRAIN_NO)

# (2) Define functions
library(lubridate)
library(dplyr)
library(purrr)
library(arrow)

# Function to split a single train's journey data by its scheduled departure date (DATDEP).
split_by_date = function(df_ref) {
  # DATDEP field indicates the date the train journey began.
  split(df_ref, df_ref$DATDEP)
}

# Core function to calculate segment travel times and stop times for a single train journey on a single day.
calculate_durations = function(df_ref){
  
  # --- Step 1: Combine and Convert Time Data ---
  
  # Create a full arrival datetime string by combining the date and time columns.
  df_ref$arrival_time = paste(df_ref$REAL_DATE_ARR,
                              df_ref$REAL_TIME_ARR,
                              sep = " ")
  # Create a full departure datetime string.
  df_ref$departure_time = paste(df_ref$REAL_DATE_DEP,
                                df_ref$REAL_TIME_DEP,
                                sep = " ")
  
  # Convert the arrival datetime strings to proper datetime objects (dmy_hms = Day-Month-Year_Hour-Minute-Second).
  df_ref$arrival_time = dmy_hms(df_ref$arrival_time,
                                tz = "Europe/Brussels")
  # Convert the departure datetime strings to proper datetime objects.
  df_ref$departure_time = dmy_hms(df_ref$departure_time,
                                  tz = "Europe/Brussels")
  
  # --- Step 2: Calculate Durations and Metadata ---
  
  # Check if the input is a list/data frame (for robustness) and has enough rows (min 3 rows for 2 segments).
  if (typeof(df_ref) == "list"){ 
    if (nrow(df_ref) > 2){
      
      df_ref[1, "stop_duration"] = as.duration(
        interval(
          start = df_ref[1, "arrival_time"],
          end = df_ref[1, "departure_time"]
        )
      )
      
      # Loop starts at the second row (i=2) because the first row has no preceding segment.
      for (i in 2:nrow(df_ref)){
        
        # Define a unique segment identifier: (Previous Stop Name)_(Current Stop Name)_(Line No.).
        df_ref[i, "segment_ref"] = paste(
          df_ref[i-1, "PTCAR_LG_NM_NL"], # Name of the previous stop
          df_ref[i, "PTCAR_LG_NM_NL"],   # Name of the current stop
          sep = "_"
        )
        
        # Calculate Segment Duration (Travel Time): Time from departure at stop i-1 to arrival at stop i.
        df_ref[i, "segment_duration"] = as.duration(
          interval(
            start = df_ref[i-1, "departure_time"],
            end = df_ref[i, "arrival_time"]
          )
        )
        
        # Calculate Stop Duration (Wait Time): Time from arrival at stop i to departure at stop i.
        df_ref[i, "stop_duration"] = as.duration(
          interval(
            start = df_ref[i, "arrival_time"],
            end = df_ref[i, "departure_time"]
          )
        )
        
        if (df_ref[i, "stop_duration"] > 0 & is.na(df_ref[i, "stop_duration"]) == FALSE) {
          df_ref[i, "train_stops_flag"] = 1
        } else {
          df_ref[i, "train_stops_flag"] = 0
        }
        
        if (df_ref[i-1, "stop_duration"] > 0 | is.na(df_ref[i-1, "stop_duration"]) == TRUE) {
          df_ref[i, "train_departs_flag"] = 1
        } else {
          df_ref[i, "train_departs_flag"] = 0
        }
        
        # Extract the hour of arrival for time-of-day binning/analysis.
        df_ref[i, "arrival_bin_1hour"] = hour(df_ref[i, "arrival_time"])
        df_ref[i, "arrival_bin_2hour"] = (hour(df_ref[i, "arrival_time"]) %/% 2) * 2
        df_ref[i, "arrival_bin_3hour"] = (hour(df_ref[i, "arrival_time"]) %/% 3) * 3
        df_ref[i, "arrival_bin_4hour"] = (hour(df_ref[i, "arrival_time"]) %/% 4) * 4
        df_ref[i, "arrival_bin_6hour"] = (hour(df_ref[i, "arrival_time"]) %/% 6) * 6
        df_ref[i, "arrival_bin_12hour"] = (hour(df_ref[i, "arrival_time"]) %/% 12) * 12
       
        df_ref[i, "departure_bin_1hour"] = hour(df_ref[i-1, "departure_time"])
        df_ref[i, "departure_bin_2hour"] = (hour(df_ref[i-1, "departure_time"]) %/% 2) * 2
        df_ref[i, "departure_bin_3hour"] = (hour(df_ref[i-1, "departure_time"]) %/% 3) * 3
        df_ref[i, "departure_bin_4hour"] = (hour(df_ref[i-1, "departure_time"]) %/% 4) * 4
        df_ref[i, "departure_bin_6hour"] = (hour(df_ref[i-1, "departure_time"]) %/% 6) * 6
        df_ref[i, "departure_bin_12hour"] = (hour(df_ref[i-1, "departure_time"]) %/% 12) * 12
        
        if (df_ref[i, "DELAY_ARR"] <= 0) {
          df_ref[i, "arrival_delayed_flag"] = 0
        } else {
          df_ref[i, "arrival_delayed_flag"] = 1
        }
        
        if (df_ref[i-1, "DELAY_DEP"] <= 0) {
          df_ref[i, "departure_delayed_flag"] = 0
        } else {
          df_ref[i, "departure_delayed_flag"] = 1
        }
        
        if (mean(df_ref[1:i,"DELAY_ARR"], na.rm = TRUE) <= 0) {
          df_ref[i, "avg_arrival_delayed_flag"] = 0
        } else {
          df_ref[i, "avg_arrival_delayed_flag"] = 1
        }
        
        if (mean(df_ref[1:(i-1), "DELAY_DEP"], na.rm = TRUE) <= 0) {
          df_ref[i, "avg_departure_delayed_flag"] = 0
        } else {
          df_ref[i, "avg_departure_delayed_flag"] = 1
        }
      }
      
      # --- Step 3: Return Filtered Results ---
      
      # Return only the rows where segment_duration was successfully calculated (i.e., not NA, excluding the first row).
      return(
        subset(
          df_ref, 
          (is.na(segment_duration) != TRUE), 
          # Select only the relevant calculated and key identification columns.
          select = c("DATDEP",
                     "TRAIN_NO",
                     "PTCAR_LG_NM_NL",
                     "segment_ref", 
                     "segment_duration", 
                     "stop_duration", 
                     "arrival_bin_1hour",
                     "arrival_bin_2hour",
                     "arrival_bin_3hour",
                     "arrival_bin_4hour",
                     "arrival_bin_6hour",
                     "arrival_bin_12hour",
                     "departure_bin_1hour",
                     "departure_bin_2hour",
                     "departure_bin_3hour",
                     "departure_bin_4hour",
                     "departure_bin_6hour",
                     "departure_bin_12hour",
                     "train_stops_flag",
                     "train_departs_flag",
                     "arrival_delayed_flag",
                     "departure_delayed_flag",
                     "avg_arrival_delayed_flag",
                     "avg_departure_delayed_flag")
        )
      )
    }
  }
}

# (3) Apply Functions and Consolidate

# Apply 'split_by_date' to every element (train journey) in the 'infrabel_raw_split' list.
# Result: A nested list structure (Train -> Date -> Stop Data)
infrabel_raw_purrr = purrr::modify(infrabel_raw_split, split_by_date)

# Iterate through the outer list (each train).
for (train_i in 1:length(infrabel_raw_purrr)){
  # For each train, apply 'calculate_durations' to the inner list (each date) using 'lapply'.
  infrabel_raw_purrr[[train_i]] = lapply(infrabel_raw_purrr[[train_i]], calculate_durations)
}

# Create a copy for the next consolidation steps.
infrabel_raw_purrr_test = infrabel_raw_purrr

# Iterate through the outer list (each train) again.
for (train_i in 1:length(infrabel_raw_purrr_test)){
  # Use dplyr::bind_rows to combine all the daily journey data frames back into one single data frame per train.
  infrabel_raw_purrr_test[[train_i]] = dplyr::bind_rows(infrabel_raw_purrr_test[[train_i]])
}

# Use dplyr::bind_rows to combine all the single-train data frames into one final, large data frame.
infrabel_segments_calculated = dplyr::bind_rows(infrabel_raw_purrr_test)

# (4) Save Output

# Save the final calculated data set to a Parquet file for efficient storage and later use.
arrow::write_parquet(
  x = infrabel_segments_calculated,
  sink = "~/Documents/Fall 2025/STAT 7100/final_project/infrabel_segments_calculated.parquet"
)
