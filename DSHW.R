
library(forecast)
library(Metrics)
library(ggplot2)

# Build training set (first 6 weeks=1008 hours)
train_ts <- window(ts_data, start = c(1, 1), end = c(6, 0))   

# The real test set is for next week (168 hours)
test_ts <- window(ts_data, start = c(6, 1), end = c(7, 0))   

# Fit the DSHW model and set the prediction length to 168 hours (1 week)
fit_dshw <- dshw(train_ts, period1 = 24, period2 = 168, h = 168)

fc_dshw <- fit_dshw  


#================================================================


# Building Training Set: The First 1128 Hours,6 weeks
train_len <- length(ts_data) - 336
train_ts <- window(ts_data, end = c(train_len %/% 168, train_len %% 168))

# Build test set: Last 2 weeks (336 hours)
test_ts <- window(ts_data, start = c(train_len %/% 168 + 1, train_len %% 168 + 1))

# DSHW 
fit_dshw <- dshw(train_ts, period1 = 24, period2 = 168, h = 336)

fc_dshw <- fit_dshw

start_time <- time(fc_dshw$mean)[1]
test_ts <- ts(tail(traffic_data$speed, 336),
              start = start_time,
              frequency = frequency(ts_data))

p_dshw <- autoplot(fc_dshw) +
  autolayer(test_ts, series = "actual", color = "red") +
  labs(title = "DSHW Forecast (Last 2 Weeks)",
       x = "Time (hour)", y = "Speed (km/h)") +
  guides(color = guide_legend(title = "图例")) +
  theme_minimal()

ggsave(filename = "./data/processed/dshw_prediction_2weeks.png",
       plot = p_dshw,
       width = 10, height = 5, dpi = 300)

pred_dshw <- fc_dshw$mean
actual <- as.numeric(test_ts)

rmse_dshw <- rmse(actual, pred_dshw)
mape_dshw <- mape(actual, pred_dshw) * 100
ei_dshw <- 1 - (sqrt(sum((actual - pred_dshw)^2)) /
                  (sqrt(sum(actual^2)) + sqrt(sum(pred_dshw^2))))

cat(sprintf("DSHW  RMSE: %.4f\n", rmse_dshw))
cat(sprintf("DSHW  MAPE: %.2f%%\n", mape_dshw))
cat(sprintf("DSHW  EI: %.4f\n", ei_dshw))

#=======================================================
p_dshw <- autoplot(fc_dshw) +
  autolayer(test_ts, series = "actual", color = "red") +
  labs(title = "DSHW",
       x = "time（hour）", y = "speed（km/h）") +
  guides(color = guide_legend(title = "图例")) +
  theme_minimal()


pred_dshw <- fc_dshw$mean
actual <- as.numeric(test_ts)

ggsave(filename = "./data/processed/dshw_prediction_week.png",
       plot = p_dshw,
       width = 10, height = 5, dpi = 300)

rmse_dshw <- rmse(actual, pred_dshw)
mape_dshw <- mape(actual, pred_dshw) * 100
ei_dshw <- 1 - (sqrt(sum((actual - pred_dshw)^2)) /
                  (sqrt(sum(actual^2)) + sqrt(sum(pred_dshw^2))))


cat(sprintf("DSHW  RMSE: %.4f\n", rmse_dshw))
cat(sprintf("DSHW  MAPE: %.2f%%\n", mape_dshw))
cat(sprintf("DSHW  EI: %.4f\n", ei_dshw))

str(fit_dshw)


