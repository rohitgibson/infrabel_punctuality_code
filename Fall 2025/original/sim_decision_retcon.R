infrabel_segment_d = arrow::read_parquet(
  "~/Documents/Fall 2025/STAT 7100/final_project/infrabel_segments_calculated.parquet"
)

infrabel_segments_valid_df = subset(
  infrabel_segment_d,
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

infrabel_segments_valid_df_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "ZAVENTEM_NOSSEGEM" 
   & train_stops_flag == 1
   & train_departs_flag == 1
   & departure_delayed_flag == 0
   & avg_departure_delayed_flag == 1
   & departure_bin_3hour == 12
   )
)

hist(
  infrabel_segments_valid_df_subset$segment_duration, breaks = 50,
  main = "Travel Time Distribution for ZAVENTEM_NOSSEGEM",
  xlim = c(0, 300),
  xlab = "Travel Time (seconds)"
  )

infrabel_segments_valid_df_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "NAMUR_NAMUR-HERBATTE"
   & train_stops_flag == 1
   # & train_departs_flag == 1
   # & departure_delayed_flag == 1
   & avg_arrival_delayed_flag == 0
   & bin_3hour == 12)
)

set.seed(7000)

seg_refs = sample(
  infrabel_segments_valid_df$segment_ref,
  size = 3
)

infrabel_segments_valid_df_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "KORTENBERG_NOSSEGEM"
   & train_stops_flag == 0
   & train_departs_flag == 0
   # & arrival_delayed_flag == 1
   & departure_delayed_flag == 0
   & avg_departure_delayed_flag == 0
   & bin_3hour == 12
   ))

infrabel_segments_valid_df_subset = subset(
  infrabel_segments_valid_df,
  (segment_ref == "GENT-DAMPOORT_BEERVELDE" 
   & train_stops_flag == 1
   & train_departs_flag == 1
   & departure_delayed_flag == 0
   & avg_departure_delayed_flag == 0
   & departure_bin_3hour == 9
   )
)

qqnorm(infrabel_segments_valid_df_subset$segment_duration)
qqline(infrabel_segments_valid_df_subset$segment_duration,
       col = "red")
hist(infrabel_segments_valid_df_subset$segment_duration, 
     breaks = 50)
summary(infrabel_segments_valid_df_subset$segment_duration)
sd(infrabel_segments_valid_df_subset$segment_duration)

set.seed(7500)

seg_refs = sample(
  infrabel_segments_valid_df$segment_ref,
  size = 3
)

infrabel_stops_valid_df_subset = subset(
  infrabel_stops_valid_df,
  (segment_ref == "BRUSSEL-KAPELLEKERK_BRUSSEL-ZUID"
   & bin_3hour == 15
   & avg_arrival_delayed_flag == 0)
)

infrabel_stops_valid_df_subset = subset(
  infrabel_stops_valid_df,
  (PTCAR_LG_NM_NL == "BRUSSEL-ZUID"
   & train_stops_flag == 0
   & arrival_bin_3hour == 12
   & arrival_delayed_flag == 0
   & avg_arrival_delayed_flag == 0
   & avg_departure_delayed_flag == 1
   )
)

qqnorm(infrabel_stops_valid_df_subset$stop_duration)
qqline(infrabel_stops_valid_df_subset$stop_duration,
       col = "red")
hist(infrabel_stops_valid_df_subset$stop_duration, 
     breaks = (nrow(infrabel_stops_valid_df_subset) / 3))
summary(infrabel_stops_valid_df_subset$stop_duration)
sd(infrabel_stops_valid_df_subset$stop_duration)


