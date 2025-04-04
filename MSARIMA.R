library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(gridExtra)
library(tseries)
library(FinTS)
library(Metrics)

# Extract data for one month (April 1st to May 31st)
traffic_hourly_2months <- traffic_hourly %>%
  filter(hour >= "2017-04-01", hour < "2017-05-31") %>%
  arrange(hour) %>%
  mutate(day_idx = as.numeric(day(hour)) - 1 + (hour(hour) / 24))

p <- ggplot(traffic_hourly_month, aes(x = day_idx, y = speed)) +
  geom_line(color = "black", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(0, 30, 1)) +
  labs(x = "idx (day)", y = "Speed (km/h)") +
  theme_bw()

# Weekend area background markers
weekends <- data.frame(
  start = c(0, 7, 14, 21, 28),
  end = c(2, 9, 16, 23, 30)
)

for (i in 1:nrow(weekends)) {
  p <- p + annotate("rect",
                    xmin = weekends$start[i],
                    xmax = weekends$end[i],
                    ymin = -Inf, ymax = Inf,
                    alpha = 0.2, fill = "gray")
}

ggsave("./data/processed/Figure3_monthly2_pattern.png",
       plot = p, width = 14, height = 6, dpi = 300)

#===========================================================


traffic_ts <- ts(traffic_hourly_2months$speed, frequency = 24)

# 168 steps (one week) first, differential
traffic_diff_168_2 <- diff(traffic_ts, lag = 168)

# Perform 24 steps (one day) of differencing on the data after 168 steps of differencing
traffic_diff_168_24_2 <- diff(traffic_diff_168_2, lag = 24)

# Draw ACF diagram after 168 steps of differential analysis
acf_168 <- ggAcf(traffic_diff_168_2, lag.max = 24*35) +
  labs(title = "ACF (168-step differenced)", x = "Lag (hours)") +
  theme_bw()

# Draw the PACF diagram after 168 differential steps
pacf_168 <- ggPacf(traffic_diff_168_2, lag.max = 24*35) +
  labs(title = "PACF (168-step differenced)", x = "Lag (hours)") +
  theme_bw()

# Draw the ACF diagram after 168 steps and then 24 steps of differentiation
acf_168_24 <- ggAcf(traffic_diff_168_24_2, lag.max = 24*7) +
  labs(title = "ACF (168-step & 24-step differenced)", x = "Lag (hours)") +
  theme_bw()

# Draw the PACF diagram after 168 steps followed by 24 steps of differential analysis
pacf_168_24 <- ggPacf(traffic_diff_168_24_2, lag.max = 24*7) +
  labs(title = "PACF (168-step & 24-step differenced)", x = "Lag (hours)") +
  theme_bw()

combined_plot <- grid.arrange(acf_168, pacf_168, 
                              acf_168_24, pacf_168_24, 
                              nrow = 2)

ggsave("./data/processed/2combined_ACF_PACF_diff_168_24.png",
       plot = combined_plot, width = 14, height = 10, dpi = 300)

#============================================================
#=======================================================

traffic_ts <- ts(traffic_hourly_month$speed, frequency = 24)

# Draw an ACF chart with a maximum delay of three weeks (24 hours x 7 days x 3 weeks)
acf_plot <- ggAcf(traffic_ts, lag.max = 24*7*3, ci = FALSE) +
  labs(x = "Lag (days)", y = "ACF") +
  scale_x_continuous(
    breaks = c(0, 7*24, 14*24, 21*24),
    labels = c("0", "7", "14", "21")
  ) +
  theme_bw()

ggsave("./data/processed/Figure4_ACF_hourly_speed.png",
       plot = acf_plot, width = 14, height = 6, dpi = 300)

#===========================================================

# ADF inspection


# Perform ADF test on the 168 step differential sequence
adf_168 <- adf.test(traffic_diff_168_2, alternative = "stationary")

# Perform ADF test on the sequence after 168+24 steps of differencing
adf_168_24 <- adf.test(traffic_diff_168_24_2, alternative = "stationary")


print(adf_168)
print(adf_168_24)

adf_result <- adf.test(traffic_diff_168_24_2, alternative = "stationary")

statistic <- round(adf_result$statistic, 4)
pvalue <- format.pval(adf_result$p.value, digits = 4, eps = .Machine$double.eps)
lag <- adf_result$parameter
nobs <- length(traffic_diff_168_24_2) - lag

# Critical value writing
critical_values <- c("-3.40705 (1%)", "-2.85789 (5%)", "-2.56717 (10%)")

cat("check results\n")
cat("-----------------------------------------------------\n")
cat(sprintf("%-30s %s\n", "Inspection items", "results"))
cat("-----------------------------------------------------\n")
cat(sprintf("%-30s %s\n", "Test Statistic Value", statistic))
cat(sprintf("%-30s %s\n", "p-value", pvalue))
cat(sprintf("%-30s %d\n", "Lags Used", lag))
cat(sprintf("%-30s %d\n", "Number of Observations Used", nobs))
cat(sprintf("%-30s %s\n", "Critical Value (1%)", critical_values[1]))
cat(sprintf("%-30s %s\n", "Critical Value (5%)", critical_values[2]))
cat(sprintf("%-30s %s\n", "Critical Value (10%)", critical_values[3]))
cat("-----------------------------------------------------\n\n")

cat("The original assumption of ADF is that time series have unit roots, that is, the series is non-stationary\n")
cat("The test statistic is", statistic, "，p", pvalue, "，Significantly less than the given level of significance（1%, 5%, 10%）。\n")
cat("Therefore, rejecting the null hypothesis indicates that the sequence has reached a steady state.\n")
cat("Subsequently, ARMA or ARIMA models can be established based on this sequence.\n")

#=====================================================================

traffic_data <- read.csv("./data/processed/target_traffic_hour.csv")
ts_data <- ts(traffic_data$speed, frequency = 24 * 7) 

# fitted model
model <- Arima(ts_data,
               order = c(1, 0, 1),
               seasonal = list(order = c(0, 0, 1), period = 24),  
               xreg = fourier(ts(ts_data, frequency=168), K=3))   

summary(model)

checkresiduals(model) 

png(filename = "./data/processed/residual_diagnostics.png",
    width = 1600, height = 1000, res = 200)

checkresiduals(fit)  

dev.off()

res <- residuals(model)

png(filename = "./data/processed/ljung_box_diagnostic.png", width = 1000, height = 800, res = 150)

par(mfrow = c(2,1))

# Residual ACF plot
acf(res, main = "ACF of Residuals")

# The p-value of Ljung Box test
lags <- 1:10
pvals <- sapply(lags, function(lag) {
  Box.test(res, lag = lag, type = "Ljung-Box", fitdf = 2)$p.value
})

plot(lags, pvals, type = "p", pch = 1, ylim = c(0,1),
     main = "p values for Ljung-Box statistic",
     xlab = "lag", ylab = "p value")
abline(h = 0.05, col = "blue", lty = 2)

dev.off()

#================================================================

#ARCH check
ArchTest(residuals(model), lags = 12)  
#=========================================
#ARCH2
res <- residuals(model)

library(rugarch)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 0)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)

fit_arch <- ugarchfit(spec = spec, data = res)

# Extract volatility sequence
vol <- sigma(fit_arch) 

# Build a time vector that corresponds to the training set time
vol_ts <- ts(vol, start = start(train_ts), frequency = frequency(train_ts))
png("arch_volatility_fit_tsplot.png", width = 1200, height = 600, res = 150)

ts.plot(vol_ts,
        col = "darkblue", lwd = 2,
        main = "ARCH (2) model conditional volatility (σ _t) varies over time",
        ylab = "Volatility (σ)", xlab = "Time (hour)")

dev.off()

# Residual ACF
png("arch_acf_residuals.png", width = 1200, height = 800, res = 200)
plot(fit_arch, which = 9)
dev.off()


# Residual distribution (density fitting plot)
png("arch_density.png", width = 1200, height = 800, res = 200)
plot(fit_arch, which = 12)
dev.off()



#===================================
set.seed(2024)  

traffic_data <- read.csv("./data/processed/target_traffic_hour.csv")

ts_data <- ts(traffic_data$speed, frequency = 168)

# Split data: The first 6 weeks (1008 hours) are used as the training set, and the remaining are used as the testing set
train_ts <- window(ts_data, start = c(1, 1), end = c(6, 0))  
test_ts <- window(ts_data, start = c(6, 1))                  

# Set the number of Fourier frequency terms
K <- 3

xreg_train <- head(fourier(ts_data, K = K), length(train_ts))         # 保证长度一致
xreg_future <- fourier(ts_data, K = K, h = length(test_ts))           # 预测未来所需项

# MARIMA ：ARIMA(1,0,1) × (0,0,1)[24]
fit <- Arima(train_ts,
             order = c(1, 0, 1),
             seasonal = list(order = c(0, 0, 1), period = 24),  # 日周期性
             xreg = xreg_train)

fc <- forecast(fit, xreg = xreg_future, h = length(test_ts))

autoplot(fc) +
  autolayer(test_ts, series = "actual", color = "red") +
  labs(title = "MSARIMA Model Results",
       x = "Time（hour）", y = "Speed（km/h）") +
  theme_minimal()

ggsave(filename = "./data/processed/prediction_vs_actual3.png",
       plot = p,
       width = 10, height = 5, dpi = 300)


# Calculate the prediction error index
pred <- fc$mean
actual <- as.numeric(test_ts)

rmse_val <- rmse(actual, pred)
mape_val <- mape(actual, pred) * 100

cat(sprintf("RMSE: %.4f\n", rmse_val))
cat(sprintf("MAPE: %.2f%%\n", mape_val))

numerator <- sqrt(sum((actual - pred)^2))
denominator <- sqrt(sum(actual^2)) + sqrt(sum(pred^2))
ei_val <- 1 - (numerator / denominator)

cat(sprintf("EI: %.4f\n", ei_val))


