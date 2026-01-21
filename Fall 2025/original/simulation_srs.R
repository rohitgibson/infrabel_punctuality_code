library(lubridate)

raw_data_aug = read.csv("~/Downloads/Data_raw_punctuality_202508.csv")
raw_data_sep = read.csv("~/Downloads/Data_raw_punctuality_202509.csv")
raw_data_sep$OP1_COD = NULL

infrabel_raw = rbind(
  raw_data_aug,
  raw_data_sep
)

infrabel_segments_calculated = arrow::read_parquet(
  "~/Documents/Fall 2025/STAT 7100/final_project/infrabel_segments_calculated.parquet"
)

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

infrabel_stops_valid_df = subset(
  infrabel_segment_d,
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

route_calc_data_sep = subset(
  raw_data_sep,
  select = c(
    "TRAIN_SERV",
    "TRAIN_NO",
    "DATDEP",
    "PTCAR_LG_NM_NL",
    "PLANNED_TIME_ARR",
    "PLANNED_TIME_DEP",
    "RELATION_DIRECTION"
  )
)

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
  
  # print(density_data)
  # print(density_breaks)
  
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
  
  # print(sample_str)
  
  sample_vec = as.vector(eval(parse(text = sample_str)))
  
  # print(sample_vec)
  # print(class(sample_vec))
  
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
  
  # paste(PTCAR_LG_NM_NL_temp,
  #       train_stops_flag_temp,
  #       arrival_delayed_flag_temp,
  #       avg_arrival_delayed_flag_temp,
  #       arrival_bin_3hour_temp)
  
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

sim_event_calculations = function (
    i,
    sim_rte_data,
    iter_col_ts,
    iter_col_arr_delay,
    iter_col_dep_delay
) {
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
  
  # print(sim_rte_data)
  
  # Calculate event_plus1 timestamp
  
  # print(i)
  
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
    
    # for (train_date in train_dates) {
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
              # print(e)
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
        
        arrow::write_parquet(
          x = sim_data_agg,
          sink = "~/Documents/Fall 2025/STAT 7100/final_project/sim_data_agg_srs.parquet"
        )
    }
  }
  
  
  return (sim_data_agg)
}

set.seed(12000)

train_run_sample_size = 100
start_event = 1
sim_runs = 100
segment_resolution = 30
stop_resolution = 30

sim_data_out_srs = sim_orch()

arrow::write_parquet(
  x = sim_data_out_srs,
  sink = "~/Documents/Fall 2025/STAT 7100/final_project/sim_data_out_srs.parquet"
)

