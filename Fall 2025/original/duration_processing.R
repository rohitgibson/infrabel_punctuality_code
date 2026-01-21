split_by_bin = function (df) {
  split(df, df$bin_hour)
}

split_by_ts_exec = function (
  df,
  duration_type
) {
  ts_0_subset = subset(df, (train_stops_flag == 0))
  ts_1_subset = subset(df, (train_stops_flag == 1))
  
  if (nrow(ts_0_subset) > 0) {
    filename = paste(
      duration_type,
      "r",
      rlang::hash(df[[1, "segment_ref"]]),
      df[[1, "bin_hour"]],
      "0",
      sep = "_"
    )
    filepath = paste(
      path_to_duration_data,
      duration_type,
      "s/valid/",
      filename,
      ".parquet",
      sep = ""
    )
    arrow::write_parquet(
      ts_0_subset,
      sink = filepath,
    )
  }
  if (nrow(ts_1_subset) > 0) {
    filename = paste(
      duration_type,
      "r",
      rlang::hash(df[[1, "segment_ref"]]),
      df[[1, "bin_hour"]],
      "1",
      sep = "_"
    )
    filepath = paste(
      path_to_duration_data,
      duration_type,
      "s/valid/",
      filename,
      ".parquet",
      sep = ""
    )
    arrow::write_parquet(
      ts_1_subset,
      sink = filepath,
    )
  }
  
}

split_by_ts_init = function (bin_list, validity, duration_type) {
  for (bin in 1:length(bin_list)) {
    split_by_ts_exec(
      bin_list[[bin]],
      duration_type
    )
  }
}

split_by_datdep = function (df) {
  split(df, df$DATDEP)
}

export_by_datdep = function(
  datdep_list,
  duration_type
) {
  for (datdep_i in 1:length(datdep_list)) {
    TRAIN_NO = datdep_list[[datdep_i]][[1, "TRAIN_NO"]]
    DATDEP = datdep_list[[datdep_i]][1, "DATDEP"]
    filename = paste(
      TRAIN_NO,
      DATDEP,
      sep = "_"
    )
    filepath = paste(
      path_to_duration_data,
      duration_type,
      "s/invalid/",
      filename,
      ".parquet",
      sep = ""
    )
    arrow::write_parquet(
      datdep_list[[datdep_i]],
      sink = filepath,
    )
  }
}

infrabel_segment_d = arrow::read_parquet(
  "~/Documents/Fall 2025/STAT 7100/final_project/infrabel_segments_calculated.parquet"
)

path_to_duration_data = "~/Documents/Fall 2025/STAT 7100/final_project/durations/"

infrabel_segments_valid_df = subset(
  infrabel_segment_d,
  (segment_duration > 0),
  select = c(
    "segment_ref",
    "segment_duration",
    "bin_hour",
    "train_stops_flag"
  )
)

infrabel_stops_valid_df = subset(
  infrabel_segment_d,
  (stop_duration >= 0),
  select = c(
    "segment_ref",
    "stop_duration",
    "bin_hour",
    "train_stops_flag"
  )
)

infrabel_segments_invalid_df = subset(
  infrabel_segment_d,
  (segment_duration <= 0),
  select = c(
    "TRAIN_NO",
    "DATDEP",
    "segment_ref",
    "segment_duration"
  )
)

infrabel_stops_invalid_df = subset(
  infrabel_segment_d,
  (stop_duration < 0),
  select = c(
    "TRAIN_NO",
    "DATDEP",
    "segment_ref",
    "stop_duration"
  )
)

infrabel_segments_valid_split = split(
  infrabel_segments_valid_df, 
  infrabel_segments_valid_df$segment_ref
)
infrabel_segments_valid_split = lapply(
  infrabel_segments_valid_split, 
  split_by_bin
)
infrabel_segments_valid_split = lapply(
  infrabel_segments_valid_split, 
  split_by_ts_init,
  duration_type = "segment"
)

infrabel_stops_valid_split = split(
  infrabel_stops_valid_df, 
  infrabel_stops_valid_df$segment_ref
)
infrabel_stops_valid_split = lapply(
  infrabel_stops_valid_split, 
  split_by_bin
)
infrabel_stops_valid_split = lapply(
  infrabel_stops_valid_split, 
  split_by_ts_init,
  duration_type = "stop"
)

infrabel_segments_invalid_split = split(
  infrabel_segments_invalid_df,
  infrabel_segments_invalid_df$TRAIN_NO
)
infrabel_segments_invalid_split = lapply(
  infrabel_segments_invalid_split,
  split_by_datdep
)
infrabel_segments_invalid_split = lapply(
  infrabel_segments_invalid_split,
  export_by_datdep,
  duration_type = "segment"
)

infrabel_stops_invalid_split = split(
  infrabel_stops_invalid_df,
  infrabel_stops_invalid_df$TRAIN_NO
)
infrabel_stops_invalid_split = lapply(
  infrabel_stops_invalid_split,
  split_by_datdep
)
infrabel_stops_invalid_split = lapply(
  infrabel_stops_invalid_split,
  export_by_datdep,
  duration_type = "stop"
)


