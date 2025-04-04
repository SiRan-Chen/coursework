target_road <- subset(road_data, link_id == 1525866939)
print(target_road)
#==================================================================
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(cluster)

##K-clustering
#read data
traffic_hourly <- read.csv("./data/processed/target_traffic_hour.csv")

traffic_hourly$hour <- ifelse(nchar(traffic_hourly$hour) == 10,
                              paste0(traffic_hourly$hour, " 00:00:00"),
                              traffic_hourly$hour)

traffic_hourly$hour <- as.POSIXct(traffic_hourly$hour, format="%Y-%m-%d %H:%M:%S")

# Clearly extract date and hour
traffic_hourly <- traffic_hourly %>%
  mutate(date = as.Date(hour),
         hour_of_day = hour(hour))

traffic_daily_matrix <- traffic_hourly %>%
  pivot_wider(id_cols = date,
              names_from = hour_of_day,
              values_from = speed,
              values_fill = NA,
              names_sort = TRUE)

# Convert to data box and set row name
traffic_daily_matrix <- as.data.frame(traffic_daily_matrix)
rownames(traffic_daily_matrix) <- traffic_daily_matrix$date
traffic_daily_matrix$date <- NULL

# Force conversion to numerical type
traffic_daily_matrix[] <- lapply(traffic_daily_matrix, function(x) as.numeric(as.character(x)))

# check
dim(traffic_daily_matrix) 
head(traffic_daily_matrix)

#z-score
traffic_scaled <- scale(traffic_daily_matrix)

sum(is.na(traffic_scaled))

# Mean imputation missing values
for (i in 1:ncol(traffic_scaled)) {
  traffic_scaled[is.na(traffic_scaled[, i]), i] <- mean(traffic_scaled[, i], na.rm = TRUE)
}

sum(is.na(traffic_scaled))

set.seed(123)

wss <- numeric(10)

for (k in 1:10) {
  km_result <- kmeans(traffic_scaled, centers = k)
  wss[k] <- km_result$tot.withinss
}

elbow_df <- data.frame(
  K = 1:10,
  WSS = wss
)

p <- ggplot(elbow_df, aes(x = K, y = WSS)) +
  geom_point(color = "blue", size = 2) +
  geom_line(color = "blue") +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method for K-means Clustering (Euclidean Distance)",
       x = "Number of Clusters (K)",
       y = "Within-cluster Sum of Squares (WSS)") +
  theme_minimal()

print(p)

ggsave(filename = "./data/elbow_method.png", plot = p, width = 8, height = 5, dpi = 300)

#=========================================================================
# Example: Cluster result with K=4
set.seed(123)
km_res_4 <- kmeans(traffic_scaled, centers = 4)
traffic_clustered_4 <- as.data.frame(traffic_scaled)
traffic_clustered_4$cluster <- factor(km_res_4$cluster)

avg_pattern_4 <- traffic_clustered_4 %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(-cluster, names_to = "hour", values_to = "speed",
               names_transform = list(hour = as.numeric))

ggplot(avg_pattern_4, aes(hour, speed, color = cluster, group = cluster)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Daily Speed Patterns (K=4)", x = "Hour", y = "Standardized Speed") +
  theme_minimal()


# K=2
km_res_2 <- kmeans(traffic_scaled, centers = 2)
silhouette_k2 <- silhouette(km_res_2$cluster, dist(traffic_scaled))
plot(silhouette_k2, main="Silhouette plot for K=2")

# K=3
km_res_3 <- kmeans(traffic_scaled, centers = 3)
silhouette_k3 <- silhouette(km_res_3$cluster, dist(traffic_scaled))
plot(silhouette_k3, main="Silhouette plot for K=3")

# K=4
km_res_4 <- kmeans(traffic_scaled, centers = 4)
silhouette_k4 <- silhouette(km_res_4$cluster, dist(traffic_scaled))
plot(silhouette_k4, main="Silhouette plot for K=4")

# View the average silhouette value

mean(silhouette_k2[,3])
mean(silhouette_k3[,3])  
mean(silhouette_k4[,3])

# Calculate and create a comparison table of results
cluster_summary <- data.frame(
  Cluster_Number = c(2, 3, 4),
  Silhouette_Avg_Width = c(
    mean(silhouette_k2[,3]),
    mean(silhouette_k3[,3]),
    mean(silhouette_k4[,3])
  ),
  Interpretation = c("Good clustering quality", 
                     "Moderate clustering quality", 
                     "Poor clustering quality")
)

print(cluster_summary)
#=====================================================================

# Use clustering results with 2 clusters
set.seed(123)
km_res_2 <- kmeans(traffic_scaled, centers = 2)

traffic_clustered_2 <- as.data.frame(traffic_scaled)
traffic_clustered_2$cluster <- factor(km_res_2$cluster)

# Daily average speed curve for each clustering pattern
library(dplyr)
library(tidyr)
library(ggplot2)

avg_pattern_2 <- traffic_clustered_2 %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(-cluster, names_to = "hour", values_to = "speed",
               names_transform = list(hour = as.numeric))

ggplot(avg_pattern_2, aes(x=hour, y=speed, color=cluster, group=cluster)) +
  geom_line(linewidth=1.2) +
  labs(title="Daily Speed Patterns (K=2)",
       x="Hour of Day",
       y="Standardized Speed") +
  theme_minimal()
ggsave(filename = "Daily_Speed_Patterns_K2.png", width = 8, height = 5, dpi = 300)

