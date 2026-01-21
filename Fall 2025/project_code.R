### (1) Library Imports

library(lubridate)
library(dplyr)
library(purrr)

### (2) Duration Calculation

# (a) Loads data for August and September 2025

raw_data_aug = read.csv("~/Downloads/Data_raw_punctuality_202508.csv")
raw_data_sep = read.csv("~/Downloads/Data_raw_punctuality_202509.csv")
raw_data_sep$OP1_COD = NULL  # Drops "OP1_COD" column from September 2025 data
                             # since it isn't present in the August 2025 data

# (b) Merges data from August and September into a single dataframe

infrabel_raw = rbind(
  raw_data_aug,
  raw_data_sep
)

# (c) Splits merged data by train number ("TRAIN_NO")

infrabel_raw_split = split(infrabel_raw, infrabel_raw$TRAIN_NO)

# (d) Splits data split by train ("infrabel_raw_split") by date of departure ("DATDEP")

## (d.i) Defines a function for splitting each train number subset by date of departure

split_by_date = function(df_ref) {
  split(df_ref, df_ref$DATDEP)
}

## (d.ii) Uses purrr::modify() to split "infrabel_raw_split" by "DATDEP"

infrabel_raw_purrr = purrr::modify(infrabel_raw_split, split_by_date)

# (e) Calculate segment and stop durations (with added context) for each subset
# in "infrabel_raw_purrr"

## (e.i) Defines a function ("calculate durations") to calculate durations for each
## individual subset in "infrabel_raw_purrr"

calculate_durations = function(df_ref){
  
  # Combines real date/time columns into a single timestamp for both
  # arrival and departure time from each measuring point
  
  df_ref$arrival_time = paste(df_ref$REAL_DATE_ARR,
                              df_ref$REAL_TIME_ARR,
                              sep = " ")
  df_ref$departure_time = paste(df_ref$REAL_DATE_DEP,
                                df_ref$REAL_TIME_DEP,
                                sep = " ")
  
  # Uses lubridate::dmy_hms to parse real arrival and departure timestamps and
  # convert each to a lubridate datetime object
  
  df_ref$arrival_time = dmy_hms(df_ref$arrival_time,
                                tz = "Europe/Brussels")
  df_ref$departure_time = dmy_hms(df_ref$departure_time,
                                  tz = "Europe/Brussels")
  
  # if-else statement handles a niche exception (related to R's inconsistent behavior
  # in how it returns the number of nested levels in a list)
  
  if (typeof(df_ref) == "list"){ 
    if (nrow(df_ref) > 2){
      
      # Calculates stop duration for the first row (i.e. the starting measuring point)
      # This is handled separately b/c there is no preceding segment duration unlike
      # every subsequent measuring point observation
      
      df_ref[1, "stop_duration"] = as.duration(
        interval(
          start = df_ref[1, "arrival_time"],
          end = df_ref[1, "departure_time"]
        )
      )
      
      # Loops from the second measurement point observation through to the last
      
      for (i in 2:nrow(df_ref)){
        
        # Defines a unique segment identifier ("segment_ref") as 
        # "(PREVIOUS MEASUREMENT POINT)_(CURRENT MEASUREMENT POINT)".
        
        df_ref[i, "segment_ref"] = paste(
          df_ref[i-1, "PTCAR_LG_NM_NL"], # Name of the previous stop
          df_ref[i, "PTCAR_LG_NM_NL"],   # Name of the current stop
          sep = "_"
        )
        
        # Calculates segment duration (travel time) as the time from when the train
        # departs from prev measurement point to the arrival time at the current
        # measurement point
        
        df_ref[i, "segment_duration"] = as.duration(
          interval(
            start = df_ref[i-1, "departure_time"],
            end = df_ref[i, "arrival_time"]
          )
        )
        
        # Calculates stop duration as the time from when the train arrives at the
        # current measurement point to when it departs
        
        df_ref[i, "stop_duration"] = as.duration(
          interval(
            start = df_ref[i, "arrival_time"],
            end = df_ref[i, "departure_time"]
          )
        )
        
        # Extracts the hours of arrival and departure for time-of-day binning/analysis.
        
        df_ref[i, "arrival_bin_1hour"] = hour(df_ref[i, "arrival_time"])                # 1 hour bins
        df_ref[i, "arrival_bin_2hour"] = (hour(df_ref[i, "arrival_time"]) %/% 2) * 2    # 2 hour bins
        df_ref[i, "arrival_bin_3hour"] = (hour(df_ref[i, "arrival_time"]) %/% 3) * 3    # 3 hour bins
        df_ref[i, "arrival_bin_4hour"] = (hour(df_ref[i, "arrival_time"]) %/% 4) * 4    # 4 hour bins
        df_ref[i, "arrival_bin_6hour"] = (hour(df_ref[i, "arrival_time"]) %/% 6) * 6    # 6 hour bins
        df_ref[i, "arrival_bin_12hour"] = (hour(df_ref[i, "arrival_time"]) %/% 12) * 12 # 12 hour bins
        df_ref[i, "departure_bin_1hour"] = hour(df_ref[i-1, "departure_time"])
        df_ref[i, "departure_bin_2hour"] = (hour(df_ref[i-1, "departure_time"]) %/% 2) * 2
        df_ref[i, "departure_bin_3hour"] = (hour(df_ref[i-1, "departure_time"]) %/% 3) * 3
        df_ref[i, "departure_bin_4hour"] = (hour(df_ref[i-1, "departure_time"]) %/% 4) * 4
        df_ref[i, "departure_bin_6hour"] = (hour(df_ref[i-1, "departure_time"]) %/% 6) * 6
        df_ref[i, "departure_bin_12hour"] = (hour(df_ref[i-1, "departure_time"]) %/% 12) * 12
        
        # Extracts information regarding the status of the train on each segment
        # for binning/analysis
        
        # Extracts whether the train is stopped at the current measurement point
        
        if (df_ref[i, "stop_duration"] > 0 & is.na(df_ref[i, "stop_duration"]) == FALSE) {
          df_ref[i, "train_stops_flag"] = 1
        } else {
          df_ref[i, "train_stops_flag"] = 0
        }
        
        # Extracts whether the train departs from the previous measurement point
        
        if (df_ref[i-1, "stop_duration"] > 0 | is.na(df_ref[i-1, "stop_duration"]) == TRUE) {
          df_ref[i, "train_departs_flag"] = 1
        } else {
          df_ref[i, "train_departs_flag"] = 0
        }
        
        # Calculates whether the train was delayed in arriving at the current
        # measurement point
        
        if (df_ref[i, "DELAY_ARR"] <= 0) {
          df_ref[i, "arrival_delayed_flag"] = 0
        } else {
          df_ref[i, "arrival_delayed_flag"] = 1
        }
        
        # Calculates whether the train was delayed in departing the previous
        # measurement point
        
        if (df_ref[i-1, "DELAY_DEP"] <= 0) {
          df_ref[i, "departure_delayed_flag"] = 0
        } else {
          df_ref[i, "departure_delayed_flag"] = 1
        }
        
        # Calculates whether the train was delayed on average upon arriving at the
        # current measurement point
        
        if (mean(df_ref[1:i,"DELAY_ARR"], na.rm = TRUE) <= 0) {
          df_ref[i, "avg_arrival_delayed_flag"] = 0
        } else {
          df_ref[i, "avg_arrival_delayed_flag"] = 1
        }
        
        # Calculates whether the train was delayed on average upon departing from
        # the previous measurement point
        
        if (mean(df_ref[1:(i-1), "DELAY_DEP"], na.rm = TRUE) <= 0) {
          df_ref[i, "avg_departure_delayed_flag"] = 0
        } else {
          df_ref[i, "avg_departure_delayed_flag"] = 1
        }
      }
      
      
      # Returns the relevant identifying and calculated columns
      
      return(
        subset(
          df_ref, 
          (is.na(segment_duration) != TRUE), 
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

## (e.ii) Applies duration calculation function to each train/date in the dataset

for (train_i in 1:length(infrabel_raw_purrr)){
  infrabel_raw_purrr[[train_i]] = lapply(infrabel_raw_purrr[[train_i]], calculate_durations)
}

# (f) Creates a combined "infrabel_segments_calculated" dataframe from the 
# calculated durations

## (f.i) Creates a copy for the next consolidation steps

infrabel_raw_purrr_test = infrabel_raw_purrr

## (f.ii) Consolidates all calculated durations for each train into a single dataframe
## per train

for (train_i in 1:length(infrabel_raw_purrr_test)){
  
  # Use dplyr::bind_rows() to combine all the daily journey dataframes back into 
  # one single data frame per train
  
  infrabel_raw_purrr_test[[train_i]] = dplyr::bind_rows(infrabel_raw_purrr_test[[train_i]])
}

## (f.iii) Combines all the single train calculated duration dataframes into one
## final, large data frame

infrabel_segments_calculated = dplyr::bind_rows(infrabel_raw_purrr_test)

### (3) Analysis of Calculated Segments

# (a) Split durations into two separate dataframes for valid segment (travel) and
# stop durations

## (a.i) Subsets calculated durations dataframe for valid (i.e. duration > 0)
## segment duration observations

infrabel_segments_valid_df = subset(
  infrabel_segments_calculated,
  (segment_duration > 0),
  select = c(
    "DATDEP",
    "TRAIN_NO",
    "PTCAR_LG_NM_NL",
    "segment_ref",
    "segment_duration",
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
    "avg_departure_delayed_flag"
  )
)

## (a.ii) Subsets calculated durations dataframe for valid (i.e. duration >= 0)
## stop duration observations

infrabel_stops_valid_df = subset(
  infrabel_segments_calculated,
  (stop_duration >= 0),
  select = c(
    "PTCAR_LG_NM_NL",
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
    "avg_departure_delayed_flag"
  )
)

# (b) Subset all valid duration observations for those occurring on
# specific measurement-point-to-measurement-point segments

mm_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "MERCHTEM_MOLLEM")
)

zn_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "ZAVENTEM_NOSSEGEM")
)

kz_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "BRUSSEL-KAPELLEKERK_BRUSSEL-ZUID")
)

# (c) Calculates summary statistics (5 num + mean + sd) for each of the subsets 
# created in step b (the output of each line has been added for convenience)

summary(mm_subset$segment_duration)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 94.0   104.0   148.0   132.6   153.0   203.0 

sd(mm_subset$segment_duration)

# [1] 24.83812

summary(zn_subset$segment_duration)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   59.00   64.00   87.42  115.00 1098.00

sd(zn_subset$segment_duration)

# [1] 49.52328

summary(kz_subset$segment_duration)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 32.0   126.0   132.0   143.7   140.0  2003.0 

sd(kz_subset$segment_duration)

# [1] 48.10599

# (d) Plots the distributions of the subsets created in step b

## (d.i) Sets up a 2x3 plotting layout

par(mfrow = c(2, 3))

## (d.ii) Plots the histograms of the distributions of the segment durations for 
## "MERCHTEM_MOLLEM," "ZAVENTEM_NOSSEGEM," and "BRUSSEL-KAPELLEKERK_BRUSSEL-ZUID"
## side-by-side on the first row of the plotting layout

hist(
  mm_subset$segment_duration, breaks = 500,
  main = "MERCHTEM_MOLLEM",
  xlim = c(0, 300),
  xlab = "Travel Time (seconds)"
)
hist(
  zn_subset$segment_duration, breaks = 5000,
  main = "ZAVENTEM_NOSSEGEM",
  xlim = c(0, 300),
  xlab = "Travel Time (seconds)"
)
hist(
  kz_subset$segment_duration, breaks = 5000,
  main = "BRUSSEL-KAPELLEKERK
  _BRUSSEL-ZUID",
  xlim = c(0, 300),
  xlab = "Travel Time (seconds)"
)

## (d.iii) Plots the Normal Q-Q plots of the distributions for the same distributions
## in the same order side-by-side on the second row of the plotting layout

qqnorm(mm_subset$segment_duration)
qqnorm(zn_subset$segment_duration)
qqnorm(kz_subset$segment_duration)

# (e) Plots the evolution of the distributions of the segment durations for the
# same segments as steps b through d as subsetting attributes are incrementally added

## (e.i) Sets up a 4x3 plotting layout for subsequent Q-Q plots and histograms

par(mfrow = c(4, 3))

## (e.ii) Subsets all valid duration observations occurring on specific 
## measurement-point-to-measurement-point segments for those where a train
## is departing from the starting measurement point and stopping at the ending
## measurement point

mm_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "MERCHTEM_MOLLEM" 
   & train_stops_flag == 1
   & train_departs_flag == 1
  )
)

zn_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "ZAVENTEM_NOSSEGEM" 
   & train_stops_flag == 1
   & train_departs_flag == 1
  )
)

kz_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "BRUSSEL-KAPELLEKERK_BRUSSEL-ZUID" 
   & train_stops_flag == 1
   & train_departs_flag == 1
  )
)

## (e.iii) Plots the Normal Q-Q plots of the distributions of the new, more
## restrictive subsets side-by-side on the first row of the layout

qqnorm(
  mm_subset$segment_duration,
  main = "MERCHTEM_MOLLEM"
)
qqnorm(
  zn_subset$segment_duration,
  main = "ZAVENTEM_NOSSEGEM"
)
qqnorm(
  kz_subset$segment_duration,
  main = "BRUSSEL-KAPELLEKERK
  _BRUSSEL-ZUID"
)

## (e.iv) Subsets the same segment durations for BOTH (1) the train departing
## AND stopping & (2) the train being both delayed in departing and delayed in
## departing on average

mm_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "MERCHTEM_MOLLEM" 
   & train_stops_flag == 1
   & train_departs_flag == 1
   & departure_delayed_flag == 1
   & avg_departure_delayed_flag == 1
  )
)

zn_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "ZAVENTEM_NOSSEGEM" 
   & train_stops_flag == 1
   & train_departs_flag == 1
   & departure_delayed_flag == 1
   & avg_departure_delayed_flag == 1
  )
)

kz_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "BRUSSEL-KAPELLEKERK_BRUSSEL-ZUID" 
   & train_stops_flag == 1
   & train_departs_flag == 1
   & departure_delayed_flag == 1
   & avg_departure_delayed_flag == 1
  )
)

## (e.v) Plots the Normal Q-Q plots of the distributions of the new, EVEN MORE
## restrictive subsets side-by-side on the second row of the layout

qqnorm(mm_subset$segment_duration)
qqnorm(zn_subset$segment_duration)
qqnorm(kz_subset$segment_duration)

## (e.vi) Subsets the same segment durations for (1) the train departing
## AND stopping, (2) the train being both delayed in departing AND delayed in
## departing on average, and (3) the train departing from the starting
## measurement point between 12 PM and 3 PM in the afternoon

mm_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "MERCHTEM_MOLLEM" 
   & train_stops_flag == 1
   & train_departs_flag == 1
   & departure_delayed_flag == 1
   & avg_departure_delayed_flag == 1
   & departure_bin_3hour == 12
  )
)

zn_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "ZAVENTEM_NOSSEGEM" 
   & train_stops_flag == 1
   & train_departs_flag == 1
   & departure_delayed_flag == 1
   & avg_departure_delayed_flag == 1
   & departure_bin_3hour == 12
  )
)

kz_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "BRUSSEL-KAPELLEKERK_BRUSSEL-ZUID" 
   & train_stops_flag == 1
   & train_departs_flag == 1
   & departure_delayed_flag == 1
   & avg_departure_delayed_flag == 1
   & departure_bin_3hour == 12
  )
)

## (e.vii) Plots the Normal Q-Q plots of the distributions of the most restrictive 
## subsets side-by-side on the third row of the layout 

qqnorm(mm_subset$segment_duration)
qqnorm(zn_subset$segment_duration)
qqnorm(kz_subset$segment_duration)

## (e.viii) Plots the histograms of the distributions of the most restrictive subsets
# side-by-side on the fourth and last row of the layout

hist(
  mm_subset$segment_duration, breaks = 500,
  main = "Histogram",
  xlim = c(0, 300),
  xlab = "Travel Time (seconds)"
)
hist(
  zn_subset$segment_duration, breaks = 5000,
  main = "Histogram",
  xlim = c(0, 300),
  xlab = "Travel Time (seconds)"
)
hist(
  kz_subset$segment_duration, breaks = 5000,
  main = "Histogram",
  xlim = c(0, 300),
  xlab = "Travel Time (seconds)"
)

# (f) Calculates summary statistics (5 num + mean + sd) for each of the most restrictive
# subsets created in step e (the output of each line has been added for convenience)

summary(mm_subset$segment_duration)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 146     150     152     153     155     172 

sd(mm_subset$segment_duration)

# [1] 4.758894

summary(zn_subset$segment_duration)

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   131.0   140.0   144.0   162.3   150.0  1098.0 

sd(zn_subset$segment_duration)

# [1] 88.02823

summary(kz_subset$segment_duration)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 121.0   126.0   128.0   131.8   131.0   289.0 

sd(kz_subset$segment_duration)

# [1] 17.91959

### (4) Simulating Travel Times

# (a) Defines functions for Monte Carlo simulation

## (a.i) Defines a function for extracting sequences of events from a subset of
## the original raw data

create_event_logs = function (
    train_schedule_df
) {
  event_log_df = tibble(
    TRAIN_SERV = character(),
    TRAIN_NO = integer(),
    PTCAR_LG_NM_NL = character(),
    event_type = character(),
    timestamp = character(),
    segment_ref = character(),
    is_origin = integer(),
    is_destination = integer(),
    .rows = 0
  )
  
  print(train_schedule_df[[1, "TRAIN_NO"]])
  
  for (i in 1:nrow(train_schedule_df)) {
    
    for (event in c("PLANNED_TIME_ARR", "PLANNED_TIME_DEP")) {
      is_origin = 0
      is_destination = 0
      
      if (train_schedule_df[[i, "PLANNED_TIME_ARR"]] == train_schedule_df[[i, "PLANNED_TIME_DEP"]]){
        if (event == "PLANNED_TIME_DEP") {
          if (i == nrow(train_schedule_df)){
            segment_ref = ""
            is_destination = 1
          } else {
            segment_ref = paste(
              train_schedule_df[[i, "PTCAR_LG_NM_NL"]],
              train_schedule_df[[i + 1, "PTCAR_LG_NM_NL"]],
              sep = "_"
            )
          }
          
          event_log_df = add_row(
            event_log_df,
            TRAIN_SERV = train_schedule_df[[i, "TRAIN_SERV"]],
            TRAIN_NO = train_schedule_df[[i, "TRAIN_NO"]],
            PTCAR_LG_NM_NL = train_schedule_df[[i, "PTCAR_LG_NM_NL"]],
            event_type = "passes",
            timestamp = train_schedule_df[[i, event]],
            segment_ref = segment_ref,
            is_origin = is_origin,
            is_destination = is_destination
          )
        }
      } else if (!is.na(train_schedule_df[[i, event]]) & train_schedule_df[[i, event]] != "") {
        if (event == "PLANNED_TIME_ARR") {
          event_type = "stops"
          segment_ref = ""
          if (i == nrow(train_schedule_df)) {
            is_destination = 1
          }
        } else {
          event_type = "departs"
          if (i == nrow(train_schedule_df)) {
            segment_ref = ""
          } else {
            segment_ref = paste(
              train_schedule_df[[i, "PTCAR_LG_NM_NL"]],
              train_schedule_df[[i + 1, "PTCAR_LG_NM_NL"]],
              sep = "_"
            )
          }
          if (i == 1) {
            is_origin = 1
          }
        }
        
        event_log_df = add_row(
          event_log_df,
          TRAIN_SERV = train_schedule_df[[i, "TRAIN_SERV"]],
          TRAIN_NO = train_schedule_df[[i, "TRAIN_NO"]],
          PTCAR_LG_NM_NL = train_schedule_df[[i, "PTCAR_LG_NM_NL"]],
          event_type = event_type,
          timestamp = train_schedule_df[[i, event]],
          segment_ref = segment_ref,
          is_origin = is_origin,
          is_destination = is_destination
        )
      }
    }
  }
  return(event_log_df)
}

## (a.ii) Defines a function for generating a simulated segment duration (i.e. a single
## data point) from the provided segment with the provided subsetting flags

simulate_seg_bin_pdf = function (
    segment_ref_temp,
    train_stops_flag_temp,
    train_departs_flag_temp,
    departure_delayed_flag_temp,
    avg_departure_delayed_flag_temp,
    departure_bin_3hour_temp
) {
  filtered_segment_data = subset(
    infrabel_segments_valid_df,
    (segment_ref == segment_ref_temp
     & train_stops_flag == train_stops_flag_temp
     & train_departs_flag == train_departs_flag_temp
     & departure_delayed_flag == departure_delayed_flag_temp
     & avg_departure_delayed_flag == avg_departure_delayed_flag_temp
     & departure_bin_3hour == departure_bin_3hour_temp
    )
  )
  
  hist_data = hist(
    filtered_segment_data$segment_duration,
    breaks = segment_resolution,
    plot = FALSE
  )
  density_data = hist_data$density
  density_breaks = hist_data$breaks
  
  # (Note this next stretch will look weird; I had to metaprogram to make this work
  # properly. Oddly this was the first idea I had to tackle this problem)
  
  sample_str = paste(
    "sample(",
    as.character(density_breaks[1]),
    ":",
    as.character(density_breaks[2]-1),
    ", 1)",
    sep = ""
  )
  
  if (length(density_breaks) > 2) {
    for (i in 2:(length(density_breaks) - 1)){
      new_sample_str = paste(
        "sample(",
        as.character(density_breaks[i]),
        ":",
        as.character(density_breaks[i+1]-1),
        ", 1)",
        sep = ""
      )
      
      sample_str = paste(
        sample_str,
        new_sample_str,
        sep = ", "
      )
    }
  }
  
  sample_str = paste(
    "c(",
    sample_str,
    ")",
    sep = ""
  )
  
  sample_vec = as.vector(eval(parse(text = sample_str)))
  
  if (length(density_breaks) > 2) {
    return ( 
      dseconds(
        sample(
          sample_vec, 1, replace = TRUE, prob = density_data
        )
      )
    )
  } else {
    return ( 
      dseconds(
        sample_vec
      )
    )
  }
}

## (a.iii) Defines a function for generating a simulated stop duration (i.e. a single
## data point) from the provided measuring point with the provided subsetting flags

simulate_stop_bin_pdf = function (
    PTCAR_LG_NM_NL_temp,
    train_stops_flag_temp,
    arrival_delayed_flag_temp,
    avg_arrival_delayed_flag_temp,
    arrival_bin_3hour_temp
) {
  filtered_stop_data = subset(
    infrabel_stops_valid_df,
    (PTCAR_LG_NM_NL == PTCAR_LG_NM_NL_temp
     & train_stops_flag == train_stops_flag_temp
     & arrival_delayed_flag == arrival_delayed_flag_temp
     & avg_arrival_delayed_flag == avg_arrival_delayed_flag_temp
     & arrival_bin_3hour == arrival_bin_3hour_temp
    )
  )
  
  hist_data = hist(
    filtered_stop_data$stop_duration,
    breaks = stop_resolution,
    plot = FALSE
  )
  density_data = hist_data$density
  density_breaks = hist_data$breaks
  sample_str = paste(
    "sample(",
    as.character(density_breaks[1]),
    ":",
    as.character(density_breaks[2]-1),
    ", 1)",
    sep = ""
  )
  
  if (length(density_breaks) > 2) {
    for (i in 2:(length(density_breaks)-1)){
      new_sample_str = paste(
        "sample(",
        as.character(density_breaks[i]),
        ":",
        as.character(density_breaks[i+1]-1),
        ", 1)",
        sep = ""
      )
      
      sample_str = paste(
        sample_str,
        new_sample_str,
        sep = ", "
      )
    }
  }
  
  sample_str = paste(
    "c(",
    sample_str,
    ")",
    sep = ""
  )
  
  sample_vec = as.vector(
    eval(
      parse(
        text = sample_str
      )
    )
  )
  
  if (length(density_breaks) > 2) {
    return ( 
      dseconds(
        sample(
          sample_vec, 1, replace = TRUE, prob = density_data
        )
      )
    )
  } else {
    return ( 
      dseconds(
        sample_vec
      )
    )
  }
}

## (a.iii) Defines a function that (1) assigns the delay of a simulated event
## to the correct category (arrival or departure delay), (2) determines how to
## calculate the timestamp of the next event, and (3) calculates the timestamp
## of the next event based on the parameters determined in parts 1 and 2

sim_event_calculations = function (
    i,
    sim_rte_data,
    iter_col_ts,
    iter_col_arr_delay,
    iter_col_dep_delay
) {
  # (Comments present b/c I spent hours debugging this)
  
  # Set current and next event types
  
  event_0_type = sim_rte_data[[i, "event_type"]]
  
  if (i != nrow(sim_rte_data)) {
    event_plus1_type = sim_rte_data[[i + 1, "event_type"]]
  } else {
    event_plus1_type = NA
  }
  
  # Set current sim time
  
  sim_time = lubridate::hms(sim_rte_data[[i, iter_col_ts]], tz = "Europe/Brussels")[1]
  sch_time = lubridate::hms(sim_rte_data[[i, "timestamp"]], tz = "Europe/Brussels")[1]
  
  # Calculate arrival and departure delays based on event_0_type
  
  if (event_0_type == "departs") {
    sim_rte_data[[i, iter_col_dep_delay]] = as.numeric(
      as.duration(sim_time) - as.duration(sch_time)
    )
  } else if (event_0_type == "passes") {
    sim_rte_data[[i, iter_col_dep_delay]] = as.numeric(
      as.duration(sim_time) - as.duration(sch_time)
    )
    sim_rte_data[[i, iter_col_arr_delay]] = as.numeric(
      as.duration(sim_time) - as.duration(sch_time)
    )
  } else if (event_0_type == "stops") {
    sim_rte_data[[i, iter_col_arr_delay]] = as.numeric(
      as.duration(sim_time) - as.duration(sch_time)
    )
  }
  
  if (!is.na(event_plus1_type)) {
    if (event_0_type %in% c("departs", "passes")) {
      sim_rte_data[[(i + 1), iter_col_ts]] = format(hms::hms(
        as.duration(sim_time) 
        + simulate_seg_bin_pdf(
          segment_ref_temp = sim_rte_data[[i, "segment_ref"]],
          train_stops_flag_temp = if (event_plus1_type == "stops") { 1 } else { 0 },
          train_departs_flag_temp = if (event_0_type == "departs") { 1 } else { 0 },
          departure_delayed_flag_temp = if (sim_rte_data[[i, iter_col_dep_delay]] > 0) { 1 } else { 0 },
          avg_departure_delayed_flag_temp = if (mean(as.numeric(sim_rte_data[start_event:i, iter_col_dep_delay]), na.rm = TRUE) > 0) { 1 } else { 0 },
          departure_bin_3hour_temp = (hour(sim_time) %/% 3) * 3
        )
      ),
      digits = 0
      )
      
    } else if (event_0_type == "stops") {
      sim_rte_data[[(i + 1), iter_col_ts]] = format(hms::hms(
        as.duration(sim_time) 
        + simulate_stop_bin_pdf(
          PTCAR_LG_NM_NL_temp = sim_rte_data[[i, "PTCAR_LG_NM_NL"]],
          train_stops_flag_temp = 1,
          arrival_delayed_flag_temp = if (sim_rte_data[[i, iter_col_arr_delay]] > 0) { 1 } else { 0 },
          avg_arrival_delayed_flag_temp = if (mean(sim_rte_data[start_event:i, iter_col_arr_delay], na.rm = TRUE) > 0) { 1 } else { 0 },
          arrival_bin_3hour_temp = (hour(sim_time) %/% 3) * 3
        )
      ),
      digits = 0
      )
    }
  }
  
  return (sim_rte_data)
  
}

## (a.iv) Defines a function that handles/orchestrates an entire end-to-end simulation run

sim_run = function (
    iter,
    sim_rte_data,
    start_time
) {
  iter_col_ts = paste(
    "sim_iter_",
    iter,
    "_timestamp",
    sep = ""
  )
  iter_col_arr_delay = paste(
    "sim_iter_",
    iter,
    "_arrival_delay",
    sep = ""
  )
  iter_col_dep_delay = paste(
    "sim_iter_",
    iter,
    "_departure_delay",
    sep = ""
  )
  
  sim_rte_data[, iter_col_ts] = NA
  sim_rte_data[, iter_col_arr_delay] = NA
  sim_rte_data[, iter_col_dep_delay] = NA
  
  sim_rte_data[[start_event, iter_col_ts]] = start_time
  
  for (i in start_event:(nrow(sim_rte_data))) {
    sim_rte_data = sim_event_calculations(
      i = i,
      sim_rte_data = sim_rte_data,
      iter_col_ts = iter_col_ts,
      iter_col_arr_delay = iter_col_arr_delay,
      iter_col_dep_delay = iter_col_dep_delay
    )
  }
  
  return (sim_rte_data)
  
}

## (a.v) Defines a function that calculates the average of the simulated arrival 
## delays and attaches them to base attributes (train, date, etc.) of the simulation run

sim_post_process = function(
    sim_rte_data_temp,
    train_actual_temp,
    sim_data_agg_temp
) {
  sim_data_final_arrival_delay = c()
  sim_data_all_arrival_delays = c()
  sim_data_all_departure_delays = c()
  
  for (col in names(sim_rte_data_temp)) {
    if (grepl("arrival", col)) {
      sim_data_final_arrival_delay = c(sim_data_final_arrival_delay, sim_rte_data_temp[[which(sim_rte_data_temp$is_destination == 1), col]])
      sim_data_all_arrival_delays = c(sim_data_all_arrival_delays, sim_rte_data_temp[, col])
    } else if (grepl("departure", col)) {
      sim_data_all_departure_delays = c(sim_data_all_departure_delays, sim_rte_data_temp[, col])
    }
  }
  
  agg_row = nrow(sim_data_agg_temp) + 1
  
  sim_data_agg_temp = add_row(
    sim_data_agg_temp,
    TRAIN_NO = train_actual_temp[[1, "TRAIN_NO"]],
    DATDEP = train_actual_temp[[1, "DATDEP"]],
    planned_start_time = train_actual_temp[[1, "PLANNED_TIME_DEP"]],
    start_time = train_actual_temp[[1, "REAL_TIME_DEP"]],
    no_events = nrow(sim_rte_data_temp),
    sim_avg_arr_delay = mean(sim_data_all_arrival_delays, na.rm = TRUE),
    act_avg_arr_delay = mean(train_actual_temp$DELAY_ARR, na.rm = TRUE),
    sim_avg_dep_delay = mean(sim_data_all_departure_delays, na.rm = TRUE),
    act_avg_dep_delay = mean(train_actual_temp$DELAY_DEP, na.rm = TRUE),
    sim_avg_final_arr_delay = mean(sim_data_final_arrival_delay, na.rm = TRUE),
    act_final_arr_delay = train_actual_temp[[nrow(train_actual_temp), "DELAY_ARR"]]
  )
  
  print("Aggregated results added to `sim_data_agg_temp`")
  print(
    paste(
      "SIM AVG ARRIVAL DELAY (s):",
      sim_data_agg_temp[[agg_row, "sim_avg_arr_delay"]],
      "ACT AVG ARRIVAL DELAY (s):",
      sim_data_agg_temp[[agg_row, "act_avg_arr_delay"]],
      "SIM AVG DEPARTURE DELAY (s):",
      sim_data_agg_temp[[agg_row, "sim_avg_dep_delay"]],
      "ACT AVG DEPARTURE DELAY (s):",
      sim_data_agg_temp[[agg_row, "act_avg_dep_delay"]],
      "SIM AVG FINAL ARRIVAL DELAY (s):",
      sim_data_agg_temp[[agg_row, "sim_avg_final_arr_delay"]],
      "ACT FINAL ARRIVAL DELAY (s):",
      sim_data_agg_temp[[agg_row, "act_final_arr_delay"]],
      sep = " "
    )
  )
  
  return (sim_data_agg_temp)
}

## (a.vi) Orchestrates all of the simulation runs for all trains and all dates
## randomly selected

sim_orch = function () {
  train_nos = sample(
    unique(raw_data_sep$TRAIN_NO), 
    size = train_run_sample_size,
    replace = FALSE
  )
  
  print(train_nos)
  
  sim_data_agg = tibble(
    TRAIN_NO = integer(),
    DATDEP = character(),
    start_time = character(),
    sim_avg_arr_delay = integer(),
    act_avg_arr_delay = integer(),
    sim_avg_dep_delay = integer(),
    act_avg_dep_delay = integer(),
    sim_avg_final_arr_delay = integer(),
    act_final_arr_delay = integer(),
    .rows = 0
  )
  
  for (train_no in train_nos) {
    train_date = sample(
      unique(
        subset(raw_data_sep, (TRAIN_NO == train_no))$DATDEP
      ),
      size = 1,
      replace = FALSE
    )
    
    train_actual = subset(infrabel_raw, (TRAIN_NO == train_no & DATDEP == train_date))
    sim_rte_data = as.data.frame(create_event_logs(train_actual))
    
    if (1 %in% sim_rte_data$is_origin & 1 %in% sim_rte_data$is_destination){
      print(sim_rte_data)
      
      start_time = train_actual[[1, "REAL_TIME_DEP"]]
      
      print(
        paste(
          "TRAIN_NO:",
          train_no,
          "DATDEP:",
          train_date,
          "Start Time:",
          start_time,
          sep = " "
        )
      )
      
      successful_runs = 0
      unsuccessful_runs = 0
      
      while (successful_runs < sim_runs & unsuccessful_runs < 100) {
        tryCatch(
          {
            sim_rte_data = sim_run(
              iter = successful_runs,
              sim_rte_data = sim_rte_data,
              start_time = start_time
            )
            print("SUCCESS!")
            successful_runs = successful_runs + 1
            unsuccessful_runs = 0
            print(successful_runs)
          },
          error = function(e) {
            unsuccessful_runs <<- unsuccessful_runs + 1
            print(
              paste(
                "FAILED ATTEMPT -",
                unsuccessful_runs,
                "@",
                Sys.time()
              )
            )
          }
        )
      }
      
      if (unsuccessful_runs < 100) {
        sim_data_agg = sim_post_process(
          sim_rte_data_temp = sim_rte_data,
          train_actual_temp = train_actual,
          sim_data_agg_temp = sim_data_agg
        )
      }
      
      # arrow::write_parquet(
      #   x = sim_data_agg,
      #   sink = "~/Documents/Fall 2025/STAT 7100/final_project/sim_data_agg_srs.parquet"
      # )
    }
  }
  
  return (sim_data_agg)
}

# (b) Simulates!

## (b.i) Sets the seed for reproducibility

set.seed(12000)

## (b.ii) Sets simulation parameters

train_run_sample_size = 100
start_event = 1
sim_runs = 100
segment_resolution = 30
stop_resolution = 30

## (b.iii) Produces the simulated output

sim_data_out_srs = sim_orch()

### (5) Analysis of simulation outputs

# (a) Calculates differences of paired samples

sim_data_out_r = sim_data_out_srs  # Creates a copy of the original output dataset

sim_data_out_r$arr_delay_diff = (sim_data_out_r$act_avg_arr_delay 
                                 - sim_data_out_r$sim_avg_arr_delay)
sim_data_out_r$dep_delay_diff = (sim_data_out_r$act_avg_dep_delay 
                                 - sim_data_out_r$sim_avg_dep_delay)
sim_data_out_r$final_delay_diff = (sim_data_out_r$act_final_arr_delay 
                                   - sim_data_out_r$sim_avg_final_arr_delay)

# (b) Creates a multi-plot layout to highlight the distributions of the differences
# in average arrival delay and final arrival delay

par(mfrow = c(2, 2))

boxplot(
  sim_data_out_r$arr_delay_diff,
  main = "Distribution of the Difference
  between Actual and Simulated 
  Average Arrival Delay",
  ylab = "Difference (seconds)",
  ylim = c(-440, 800)
)
boxplot(
  sim_data_out_r$final_delay_diff,
  main = "Distribution of the Difference
  between Simulated Average and Actual 
  Final Arrival Delay",
  ylab = "Difference (seconds)",
  ylim = c(-440, 800)
)

qqnorm(sim_data_out_r$arr_delay_diff)
qqnorm(sim_data_out_r$final_delay_diff)

# (c) Calculates summary statistics for the differences in average arrival delay
# and final arrival delay (outputs included for reference)

summary(sim_data_out_r$arr_delay_diff)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -290.935  -95.832   10.835    4.576   97.815  419.977 

sd(sim_data_out_r$arr_delay_diff)

# [1] 150.3998

summary(sim_data_out_r$final_delay_diff)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -423.21  -98.72  -11.60   46.71  133.55  783.32 

sd(sim_data_out_r$final_delay_diff)

# [1] 261.3944

# (d) Performs a hypothesis test for mean difference in paired samples b/w Sim & Act 
# Avg Arrival Delay using t-dist b/c sigma unknown

# Calculate sample mean and standard deviation for the differences between
# the two samples

s = sd(sim_data_out_r$arr_delay_diff)
n = length(sim_data_out_r$arr_delay_diff)
xbar = mean(sim_data_out_r$arr_delay_diff)
alpha = 1 - 0.90

# Calculate the t critical value and margin of error

t_crit = qt(1 - (alpha / 2), df = (n - 1)) # Calculates t critical value with `qt`
E = t_crit * (s / sqrt(n))                 # Calculates margin of error

# Determine the upper and lower limits for the 90% confidence interval

lower_bound = xbar - E # Calculates the lower confidence interval limit
upper_bound = xbar + E # Calculates the upper confidence interval limit

# Display the upper and lower limits for the 90% confidence interval

print(
  paste(
    "Lower Bound:",
    lower_bound,
    "Upper Bound:",
    upper_bound,
    sep = " "
  )
)

# (e) Performs a hypothesis test for mean difference in paired samples b/w Sim & 
#Act Final Arrival Delay using t-dist b/c sigma unknown

# Calculate sample mean and standard deviation for the differences between
# the two samples

s = sd(sim_data_out_r$final_delay_diff)
n = length(sim_data_out_r$final_delay_diff)
xbar = mean(sim_data_out_r$final_delay_diff)
alpha = 1 - 0.90

# Calculate the t critical value and margin of error

t_crit = qt(1 - (alpha / 2), df = (n - 1)) # Calculates t critical value with `qt`
E = t_crit * (s / sqrt(n))                 # Calculates margin of error

# Determine the upper and lower limits for the 90% confidence interval

lower_bound = xbar - E # Calculates the lower confidence interval limit
upper_bound = xbar + E # Calculates the upper confidence interval limit

# Display the upper and lower limits for the 90% confidence interval

print(
  paste(
    "Lower Bound:",
    lower_bound,
    "Upper Bound:",
    upper_bound,
    sep = " "
  )
)


