library(tibble)
library(purrr)
library(arrow)

infrabel_raw = read.csv("~/Downloads/Data_raw_punctuality_202509.csv")

# SUBSET FOR RELEVANT COLUMNS

infrabel_raw_subset = subset(
  infrabel_raw,
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

# SPLIT BY TRAIN

infrabel_raw_subset_split = split(infrabel_raw_subset, infrabel_raw_subset$TRAIN_NO)

# SPLIT BY DAY

split_by_day = function (df) {
  split(df, df$DATDEP)
}

infrabel_raw_subset_split = modify(infrabel_raw_subset_split, split_by_day)

# DROP EVERYTHING BUT THE FIRST INSTANCE

for (train_i in 1:length(infrabel_raw_subset_split)) {
  infrabel_raw_subset_split[[train_i]] = infrabel_raw_subset_split[[train_i]][[1]]
}

# CREATE EVENT LOGS

create_event_logs = function (train_schedule_df) {
  
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

infrabel_raw_subset_split = lapply(infrabel_raw_subset_split, create_event_logs)

export_path = "~/Documents/Fall 2025/STAT 7100/final_project/extracted_routes/"

extracted_route_export = function(event_log_df) {
  train_no = as.character(event_log_df[[1, "TRAIN_NO"]])
  filename = paste(
    "route",
    train_no,
    sep = "_"
  )
  filepath = paste(
    export_path,
    filename,
    ".parquet",
    sep = ""
  )
  arrow::write_parquet(
    event_log_df,
    sink = filepath
  )
}

 lapply(infrabel_raw_subset_split, extracted_route_export)

