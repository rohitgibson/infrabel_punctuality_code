sim_data_out_r = arrow::read_parquet(
  "~/Documents/Fall 2025/STAT 7100/final_project/sim_data_out_srs.parquet"
)

library(lubridate)

for (row in 1:nrow(sim_data_out_r)) {
  train_actual = subset(infrabel_raw, (TRAIN_NO == sim_data_out_r[[row, "TRAIN_NO"]] & DATDEP == sim_data_out_r[[row, "DATDEP"]]))
  train_route = as.data.frame(create_event_logs(train_actual))
  sim_data_out_r[[row, "no_events"]] = nrow(train_route)
  sim_data_out_r[[row, "init_delay"]] = as.duration(
                                          interval(
                                            start = hms(train_route[[1, "timestamp"]]),
                                            end = hms(sim_data_out_r[[row, "start_time"]])
                                          )
                                        )
  sim_data_out_r[[row, "planned_duration"]] = as.duration(
                                                interval(
                                                  start = dmy_hms(
                                                    paste(
                                                      sim_data_out_r[[row, "DATDEP"]],
                                                      train_route[[1, "timestamp"]]
                                                      )
                                                    ),
                                                  end = dmy_hms(
                                                    paste(
                                                      sim_data_out_r[[row, "DATDEP"]],
                                                      train_route[[nrow(train_route), "timestamp"]]
                                                    )
                                                  )
                                                )
                                              )
}

par(mfrow = c(2, 2))

sim_data_out_r$arr_delay_diff = (sim_data_out_r$act_avg_arr_delay 
                                 - sim_data_out_r$sim_avg_arr_delay)
sim_data_out_r$dep_delay_diff = (sim_data_out_r$act_avg_dep_delay 
                                 - sim_data_out_r$sim_avg_dep_delay)
sim_data_out_r$final_delay_diff = (sim_data_out_r$act_final_arr_delay 
                                   - sim_data_out_r$sim_avg_final_arr_delay)

boxplot(
  sim_data_out_r$arr_delay_diff, 
  # breaks = 20,
  # xlim = c(-300, 500),
  main = "Distribution of the Difference
  between Actual and Simulated 
  Average Arrival Delay",
  ylab = "Difference (seconds)",
  ylim = c(-440, 800)
)
# hist(sim_data_out_r$dep_delay_diff, breaks= 20)
boxplot(
  sim_data_out_r$final_delay_diff, 
  # breaks = 20,
  # xlim = c(-500, 500),
  main = "Distribution of the Difference
  between Simulated Average and Actual 
  Final Arrival Delay",
  ylab = "Difference (seconds)",
  ylim = c(-440, 800)
)
qqnorm(sim_data_out_r$arr_delay_diff)
# qqnorm(sim_data_out_r$dep_delay_diff)
qqnorm(sim_data_out_r$final_delay_diff)

summary(sim_data_out_r$arr_delay_diff)
sd(sim_data_out_r$arr_delay_diff)

summary(sim_data_out_r$final_delay_diff)
sd(sim_data_out_r$final_delay_diff)

boxplot(sim_data_out_r$arr_delay_diff)
boxplot(sim_data_out_r$dep_delay_diff)
boxplot(sim_data_out_r$final_delay_diff)

sim_data_agg_srs_temp_b = arrow::read_parquet(
  "~/Documents/Fall 2025/STAT 7100/final_project/sim_data_agg_srs.parquet"
)

qqnorm(sim_data_out_r$arr_delay_diff)

# Hypothesis Test for Significant Difference b/w Sim & Act Avg Arrival Delay

# Using t-dist b/c sigma unknown

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

# Hypothesis Test for Significant Difference b/w Sim & Act Final Arrival Delay

# Using t-dist b/c sigma unknown

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

