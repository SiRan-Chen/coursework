library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(ggplot2)
library(scales)
#===================================================================
##Convert to 1-hour data

# Read data
traffic_15min <- read_excel("./data/target_data.xlsx", col_names = FALSE)

# road_segment_id, time_stamp, traffic_speed
traffic_15min <- traffic_15min %>%
  separate(col = ...1, into = c("road_segment_id", "time_stamp", "traffic_speed"), sep = ",", convert = TRUE)

start_time <- as.POSIXct("2017-04-01 00:00:00", tz = "Asia/Shanghai")
traffic_15min <- traffic_15min %>%
  mutate(datetime = start_time + minutes(15 * time_stamp))

head(traffic_15min)

traffic_hourly <- traffic_15min %>%
  mutate(hour = floor_date(datetime, unit = "hour")) %>%
  group_by(hour) %>%
  summarise(speed = mean(traffic_speed, na.rm = TRUE))

head(traffic_hourly)

# Output the number of aggregated result records (expected: 61 days x 24 hours=approximately 1464 records)
print(nrow(traffic_hourly))

# save
write.csv(traffic_hourly, "./data/processed/target_traffic_hour.csv", row.names = FALSE)

#======================================================================
##Visualization of Road Speed Changes

traffic_15min <- read_excel("./data/target_data.xlsx", col_names = FALSE) %>%
  separate(col = ...1, 
           into = c("road_segment_id", "time_stamp", "traffic_speed"), 
           sep = ",", convert = TRUE) %>%
  mutate(datetime = as.POSIXct("2017-04-01 00:00:00", tz = "Asia/Shanghai") + minutes(15 * time_stamp))

any(duplicated(traffic_15min$datetime))

traffic_hourly <- traffic_15min %>%
  mutate(hour = floor_date(datetime, unit = "hour")) %>%
  group_by(hour) %>%
  summarise(speed = mean(traffic_speed, na.rm = TRUE))

Sys.setlocale("LC_TIME", "C")

p <- ggplot() +
  geom_line(data = traffic_15min, 
            aes(x = datetime, y = traffic_speed), 
            color = "grey60", linewidth = 0.3) +
  geom_line(data = traffic_hourly, 
            aes(x = hour, y = speed), 
            color = "red", linewidth = 0.8) +
  labs(
    x = "Date",
    y = "Speed (km/h)"
  ) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 day") +
  theme_bw()

# save
ggsave("./data/processed/Figure2_final_english.png",
       plot = p, width = 20, height = 6, dpi = 300)
