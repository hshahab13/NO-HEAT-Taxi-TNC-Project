###########################################################################################################
#Script Metadata
#Created by: Hajra Shahab
#About: 
#  --
#Version Log: 
#  --HS- 09/02/2025-- 
#Output: 
# --

#Files Output: 

###########################################################################################################
# Clear R environment every time you run a new batch 
rm(list = ls())

# Load required libraries
liby <- c("sqldf", "ggplot2", "rstudioapi", "questionr", "tidyr", "here")
lapply(liby, require, character.only = TRUE)

# Set working directory
here::i_am("NO_HEAT_TAXI_TNC.Rproj") 

# Option settings
options(scipen = 100000)
options(sqldf.driver = "SQLite")
theme_set(theme_bw())

############################################################################################################

#read heat index files from heat_index analysis 

hot_days_2018 <- read.csv(here("Processed Data", "heat_index", "daily_combined_data_HI_precipitation_2018_2025-09-02.csv"), stringsAsFactors = FALSE)
hot_days_2019 <- read.csv(here("Processed Data", "heat_index", "daily_combined_data_HI_precipitation_2019_2025-09-02.csv"), stringsAsFactors = FALSE)
hot_days_2023 <- read.csv(here("Processed Data", "heat_index", "daily_combined_data_HI_precipitation_2023_2025-09-02.csv"), stringsAsFactors = FALSE)
hot_days_2024 <- read.csv(here("Processed Data", "heat_index", "daily_combined_data_HI_precipitation_2024_2025-09-02.csv"), stringsAsFactors = FALSE)
#hot_days_2025 <- read.csv(here("Processed Data", "heat_index", "daily_combined_data_HI_precipitation_2025_2025-09-02.csv"), stringsAsFactors = FALSE)

hot_days_2018$date <- as.Date(hot_days_2018$date)
hot_days_2018$day_of_week <- weekdays(hot_days_2018$date)

hot_days_2018_select_days <- hot_days_2018[!(hot_days_2018$day_of_week %in% c('Saturday', 'Sunday')),]

hot_days_2018_hot_days <- hot_days_2018_select_days %>%
  mutate(
    hot_1 = case_when(
      hi_max >= hi_max.90perc & hi_max >= 80 ~ 1,
      TRUE ~ 0
    ),
    hot_2 = case_when(
      hi_max >= 90 ~ 1,
      TRUE ~ 0
    ),
    hot_3 = case_when(
      hi_max >= 100 ~ 1,
      TRUE ~ 0
    ))

list_hot_day_complete <- hot_day_list_def1_2024[hot_day_list_def1_2024$complete == 0,]$date_hot
list_prior_day_complete <- hot_day_list_def1_2024[hot_day_list_def1_2024$complete == 0,]$date_prior
list_after_day_complete <- hot_day_list_def1_2024[hot_day_list_def1_2024$complete == 0,]$date_after
# 
# list_hot_day_complete <- hot_day_list_def1_2024$date_hot
# list_prior_day_complete <- hot_day_list_def1_2024$date_prior
# list_after_day_complete <- hot_day_list_def1_2024$date_after
# 
# avg_hot_day_complete <- round(mean(daily_trips[daily_trips$trip_date %in% list_hot_day_complete,]$num_trips))
# avg_prior_day_complete <- round(mean(daily_trips[daily_trips$trip_date %in% list_prior_day_complete,]$num_trips))
# avg_after_day_complete <- round(mean(daily_trips[daily_trips$trip_date %in% list_after_day_complete,]$num_trips))

list_hot_day_complete <- hot_day_list_def1_2024$date_hot
list_control_day_complete <- unique(c(hot_day_list_def1_2024$date_prior, hot_day_list_def1_2024$date_after))
avg_hot_day_complete <- round(mean(daily_trips[daily_trips$trip_date %in% list_hot_day_complete,]$num_trips))
avg_control_day_complete <- round(mean(daily_trips[daily_trips$trip_date %in% list_control_day_complete,]$num_trips))


#filter rainy days and special events from dataset
feasible_days_yellow_trips_2019 <- spatial_daily_yellow_trips_2019_clean %>%
  filter(rainy_day == 0) %>%
  filter(special_events == 0) 
feasible_days_yellow_trips_2019_List <- as.list(feasible_days_yellow_trips_2019$date)

hot_daily_feasible_yellow_trips_2019 <- feasible_days_yellow_trips_2019 %>%
  filter(hot_day_warning == 1) 
hot_dates <- as.list(hot_daily_feasible_yellow_trips_2019$date)

list_hot <- c(); list_prior <- c(); list_after <- c();

for(hot_date in hot_dates){
  
  date_prior <- as.character(as.Date(hot_date) - 7)
  date_after <- as.character(as.Date(hot_date) + 7)
  
  date_prior_hot_check <- ifelse(date_prior %in% hot_dates & date_prior %in% feasible_days_yellow_trips_2019_List, 1,
                                 ifelse(!(date_prior %in% feasible_days_yellow_trips_2019_List), 1, 0))
  date_after_hot_check <- ifelse(date_after %in% hot_dates & date_after %in% feasible_days_yellow_trips_2019_List, 1,
                                 ifelse(!(date_after %in% feasible_days_yellow_trips_2019_List), 1, 0))
  
  list_hot <- c(list_hot, hot_date)
  
  if(date_prior_hot_check == 1){
    list_prior <- c(list_prior, NA)
  } else {
    list_prior <- c(list_prior, date_prior)
  }
  
  if(date_after_hot_check == 1){
    list_after <- c(list_after, NA)
  } else {
    list_after <- c(list_after, date_after)
  }
}

df_output <- do.call('rbind', Map(data.frame, date_hot = list_hot, date_prior = list_prior, date_after = list_after))
rownames(df_output) <- NULL

fwrite(df_output, here(here("Intermediate Output", 
                            "yellow_taxi",
                            "list of complete and incomplete sets_warn_2019.csv")))


hot_day_list_def1_2024$complete <- ifelse(hot_day_list_def1_2024$date_prior == "" |
                                            hot_day_list_def1_2024$date_after == "", 0, 1)

hot_day_list_def1_2024 <- hot_day_list_def1_2024[!(hot_day_list_def1_2024$date_hot %in% c('2024-07-07')),]
