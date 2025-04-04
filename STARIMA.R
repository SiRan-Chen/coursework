library(sf)
library(spdep)
library(dbscan)

# Read MapInfo spatial data
road_sf <- st_read("./data/raw/Q-Traffic Dataset/panwang/R.mif")
head(road_sf)
save(road_sf, file = "./data/processed/road_sf.RData")
load("./data/processed/road_sf.RData")


# WGS84 (EPSG:4326)
road_sf <- st_set_crs(road_sf, 4326)

# Calculate first-order adjacency matrix
road_adjacent_1 <- st_touches(road_sf, sparse = TRUE)

str(road_adjacent_1)

save(road_adjacent_1, file="./data/processed/road_adjacent_1.RData")
load("./data/processed/road_adjacent_1.RData")

# Define a function to obtain a second-order adjacency matrix
get_second_order_nb <- function(nb_list) {
  second_order_nb <- lapply(seq_along(nb_list), function(i) {
    unique(unlist(nb_list[nb_list[[i]]]))
  })
  return(second_order_nb)
}

road_adjacent_2 <- get_second_order_nb(road_adjacent_1)

save(road_adjacent_2, file="./data/processed/road_adjacent_2.RData")
load("./data/processed/road_adjacent_2.RData")

#===================================================
# Extract the coordinates of the center point of the road
coords <- st_coordinates(st_centroid(road_sf))

# Using dbscan for fast nearest neighbor calculation (k=2)
knn_result <- dbscan::kNN(coords, k=2)

# Construct nb objects directly from the results of kNN
neighbors <- split(knn_result$id, rep(1:nrow(coords), each=2))

road_nb_1 <- neighbors
class(road_nb_1) <- "nb"
attr(road_nb_1, "region.id") <- as.character(1:length(road_nb_1))
attr(road_nb_1, "type") <- "knn"
attr(road_nb_1, "call") <- match.call()

summary(road_nb_1, zero.policy=TRUE)
save(road_nb_1, file="./data/processed/road_nb_1.RData")
load("./data/processed/road_nb_1.RData")

# Constructing a spatial weight matrix
road_listw_1 <- nb2listw(road_nb_1, style="W", zero.policy=TRUE)

save(road_listw_1, file="./data/processed/road_listw_1.RData")
load("./data/processed/road_listw_1.RData")

#=================================================

# Load first-order adjacency matrix
load("./data/processed/road_nb_1.RData")

# Constructing a second-order adjacency matrix based on first-order neighbors
road_nb_2 <- nblag(road_nb_1, 2)[[2]]

summary(road_nb_2)

save(road_nb_2, file="./data/processed/road_nb_2.RData")

# Constructing a second-order spatial weight matrix
road_listw_2 <- nb2listw(road_nb_2, style="W", zero.policy=TRUE)

save(road_listw_2, file="./data/processed/road_listw_2.RData")

#======================================================================

library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

data_dir <- "./data/raw/Q-Traffic Dataset/traffic_speed_sub-dataset.v2/split_files/"

# all xlsx files
xlsx_files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE)

xlsx_files <- xlsx_files[!grepl("/~\\$", xlsx_files)]

print(xlsx_files)

read_and_process_file <- function(file_path) {
  data <- read_excel(file_path, col_names = FALSE)
  
  # Split and standardize data
  data_clean <- data %>%
    separate(col = 1, into = c("link_id", "time_index", "speed"), sep = ",", convert = TRUE) %>%
    mutate(speed = as.numeric(speed),
           time_index = as.integer(time_index),
           link_id = as.character(link_id))
  
  start_time <- ymd_hms("2017-04-01 00:00:00") 
  
  data_clean <- data_clean %>%
    mutate(time = start_time + minutes((time_index - min(time_index)) * 15))
  
  return(data_clean)
}

library(purrr)
library(dplyr)

# Set batch size
batch_size <- 5

# Split all file paths into multiple batches
xlsx_files_batches <- split(xlsx_files, ceiling(seq_along(xlsx_files)/batch_size))

road_speed_summary <- data.frame()

for (i in seq_along(xlsx_files_batches)) {
  
  cat("Now", i, "file，all", length(xlsx_files_batches[[i]]), "files\n")
  
  batch_data <- map_dfr(xlsx_files_batches[[i]], read_and_process_file)
  
  batch_avg_speed <- batch_data %>%
    group_by(link_id) %>%
    summarise(
      avg_speed_batch = mean(speed, na.rm = TRUE),
      n_records_batch = n(),
      .groups = "drop"
    )
  
  if (i == 1) {
   
    road_speed_summary <- batch_avg_speed %>%
      rename(avg_speed = avg_speed_batch, total_records = n_records_batch)
  } else {
    
    road_speed_summary <- road_speed_summary %>%
      full_join(batch_avg_speed, by = "link_id") %>%
      mutate(
        avg_speed = rowMeans(select(., avg_speed, avg_speed_batch), na.rm = TRUE),
        total_records = rowSums(select(., total_records, n_records_batch), na.rm = TRUE)
      ) %>%
      select(link_id, avg_speed, total_records)
  }
  
  # save
  save(road_speed_summary, file = paste0("./data/processed/road_speed_summary_batch_", i, ".RData"))
  
  cat("number", i, "have completed，save\n")
}

save(road_speed_summary, file="./data/processed/road_speed_summary_final.RData")
list.files("./data/processed/")
load("./data/processed/road_speed_summary_final.RData")
#============================================================

# Convert sf objects to data.frame for merging
road_sf_df <- road_sf %>% 
  st_set_geometry(NULL) %>%
  mutate(id = as.character(id))

road_sf_merged <- road_sf_df %>%
  left_join(road_speed_summary, by = c("id" = "link_id"))

road_sf_final <- road_sf %>%
  select(id, geometry) %>%
  left_join(road_sf_merged, by = "id")

head(road_sf_final)

# Delete roads with missing avd_speed
road_sf_final <- road_sf_final %>%
  filter(!is.na(avg_speed))

nrow(road_sf_final)
#==============================================

library(spdep)

# Recalculate the center coordinates
coords_final <- st_coordinates(st_centroid(road_sf_final))

# Refactoring the K-nearest neighbor space weight matrix
knn_final <- knearneigh(coords_final, k = 2)
nb_final <- knn2nb(knn_final)

# Clearly generate spatial weight matrix
road_listw_final <- nb2listw(nb_final, style = "W", zero.policy = TRUE)

global_moran_final <- moran.test(road_sf_final$avg_speed, road_listw_final, zero.policy = TRUE)

print(global_moran_final)

#===============================================================
library(readxl)
library(dplyr)
library(purrr)
library(lubridate)

data_dir <- "./data/raw/Q-Traffic Dataset/traffic_speed_sub-dataset.v2/split_files/"
xlsx_files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE)
xlsx_files <- xlsx_files[!grepl("/~\\$", xlsx_files)]


read_hourly_speed <- function(file_path) {
  data <- read_excel(file_path, col_names = FALSE) %>%
    separate(col = 1, into = c("link_id", "time_index", "speed"), sep = ",", convert = TRUE) %>%
    mutate(speed = as.numeric(speed),
           time_index = as.integer(time_index),
           link_id = as.character(link_id),
           start_time = ymd_hms("2017-04-01 00:00:00"),
           time = start_time + minutes((time_index - min(time_index))*15),
           hour = floor_date(time, "hour")) %>%
    group_by(link_id, hour) %>%
    summarise(hourly_speed = mean(speed, na.rm = TRUE), .groups = "drop")
  
  return(data)
}
batch_size <- 5
xlsx_files_batches <- split(xlsx_files, ceiling(seq_along(xlsx_files)/batch_size))

for (i in seq_along(xlsx_files_batches)) {
  cat("Now", i, "file\n")
  
  traffic_hourly_batch <- map_dfr(xlsx_files_batches[[i]], read_hourly_speed)
  
  save(traffic_hourly_batch, 
       file = paste0("./data/processed/traffic_hourly_batch_", i, ".RData"))
  
  cat("number", i, "have completed\n")
}

batch_files <- list.files("./data/processed/", 
                          pattern="traffic_hourly_batch_.*\\.RData",
                          full.names = TRUE)

traffic_hourly_all <- map_dfr(batch_files, ~{
  load(.x)
  traffic_hourly_batch
})

save(traffic_hourly_all, file = "./data/processed/traffic_hourly_all.RData")

#============================================================

load("./data/processed/traffic_hourly_all.RData")

#===============================================================
library(dplyr)
library(tidyr)

# Convert to STARIMA matrix form
traffic_matrix <- traffic_hourly_all %>%
  pivot_wider(names_from = link_id, values_from = hourly_speed) %>%
  arrange(hour)

# Check the data matrix (rows: time, columns: roads)
head(traffic_matrix)
dim(traffic_matrix)
#===================================================================


# 1525866939
target_road <- "1525866939"
target_series <- traffic_matrix[[target_road]]

length(target_series)

load("./data/processed/road_listw_2.RData")

head(traffic_matrix)

spatial_data <- traffic_matrix %>% select(-hour)

# Clearly create a spatial lag matrix
spatial_lag_matrix <- apply(spatial_data, 1, function(x) {
  lag.listw(road_listw_final, x, zero.policy = TRUE)
}) %>% t()

# Check the structure of the spatial lag matrix
dim(spatial_lag_matrix)

# Set clear column names (matching with road IDs)
colnames(spatial_lag_matrix) <- colnames(spatial_data)


library(lubridate)
library(dplyr)

traffic_matrix <- traffic_matrix %>% arrange(hour)

# Set the segmentation date for the training and testing sets
train_end_date <- ymd_hms("2017-05-12 23:00:00")

train_data <- traffic_matrix %>% filter(hour <= train_end_date)

test_data <- traffic_matrix %>% filter(hour > train_end_date)

range(train_data$hour)
range(test_data$hour)
#===========================================================

library(dplyr)
library(tidyr)

train_matrix <- train_data %>%
  pivot_longer(-hour, names_to = "link_id", values_to = "speed") %>%
  pivot_wider(names_from = link_id, values_from = speed) %>%
  arrange(hour)

head(train_matrix)
library(dplyr)
library(tidyr)

train_matrix <- train_data %>%
  pivot_longer(-hour, names_to = "link_id", values_to = "speed") %>%
  pivot_wider(names_from = link_id, values_from = speed) %>%
  arrange(hour)

head(train_matrix)

train_spatial_data <- train_matrix %>% select(-hour)

# Clearly create a spatial lag matrix
train_spatial_lag <- apply(train_spatial_data, 1, function(x){
  lag.listw(road_listw_final, x, zero.policy = TRUE)
}) %>% t()

colnames(train_spatial_lag) <- colnames(train_spatial_data)

target_series_train <- train_matrix[["1525866939"]]

# Extract the spatial lag sequence of the target road
spatial_lag_train <- train_spatial_lag[, "1525866939"]

starima_model <- auto.arima(target_series_train, xreg = spatial_lag_train)

summary(starima_model)

#====================================================================
# Clearly construct a spatiotemporal data matrix for the test set
test_matrix <- test_data %>%
  pivot_longer(-hour, names_to = "link_id", values_to = "speed") %>%
  pivot_wider(names_from = link_id, values_from = speed) %>%
  arrange(hour)

test_spatial_data <- test_matrix %>% select(-hour)

# Calculation of Test Set Spatial Lag Matrix
test_spatial_lag <- apply(test_spatial_data, 1, function(x){
  lag.listw(road_listw_final, x, zero.policy = TRUE)
}) %>% t()

colnames(test_spatial_lag) <- colnames(test_spatial_data)

spatial_lag_test <- test_spatial_lag[, "1525866939"]

#=============================================================

# Using STARIMA model to predict test set
forecast_test <- forecast(starima_model, xreg = spatial_lag_test)

print(forecast_test)
plot(forecast_test, 
     main = "STARIMA Model Forecast on Test Set",
     xlab = "Time (Hours)",
     ylab = "Predicted Traffic Speed")

# Clearly calculate the prediction error index
rmse <- sqrt(mean((actual_test - forecast_test$mean)^2, na.rm = TRUE))
mae <- mean(abs(actual_test - forecast_test$mean), na.rm = TRUE)
mape <- mean(abs((actual_test - forecast_test$mean)/actual_test), na.rm = TRUE) * 100

cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "%\n")


