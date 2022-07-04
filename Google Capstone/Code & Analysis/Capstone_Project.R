library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(dplyr)
getwd()
may_2020 <- read_csv("2020_05-divvy-tripdata.csv")
jun_2020 <- read_csv("2020_06-divvy-tripdata.csv")
jul_2020 <- read_csv("2020_07-divvy-tripdata.csv")
aug_2020 <- read_csv("2020_08-divvy-tripdata.csv")
sep_2020 <- read_csv("2020_09-divvy-tripdata.csv")
oct_2020 <- read_csv("2020_10-divvy-tripdata.csv")
nov_2020 <- read_csv("2020_11-divvy-tripdata.csv")
dec_2020 <- read_csv("2020_12-divvy-tripdata.csv")
jan_2021 <- read_csv("2021_01-divvy-tripdata.csv")
feb_2021 <- read_csv("2021_02-divvy-tripdata.csv")
mar_2021 <- read_csv("2021_03-divvy-tripdata.csv")
apr_2021 <- read_csv("2021_04-divvy-tripdata.csv")
nov_2020 <- mutate(nov_2020, start_station_id = as.character(start_station_id) ,
                   end_station_id = as.character(end_station_id))
oct_2020 <- mutate(oct_2020, start_station_id = as.character(start_station_id) ,
                   end_station_id = as.character(end_station_id))
sep_2020 <- mutate(sep_2020, start_station_id = as.character(start_station_id) ,
                   end_station_id = as.character(end_station_id))
aug_2020 <- mutate(aug_2020, start_station_id = as.character(start_station_id) ,
                   end_station_id = as.character(end_station_id))
jul_2020 <- mutate(jul_2020, start_station_id = as.character(start_station_id) ,
                   end_station_id = as.character(end_station_id))
jun_2020 <- mutate(jun_2020, start_station_id = as.character(start_station_id) ,
                   end_station_id = as.character(end_station_id))
may_2020 <- mutate(may_2020, start_station_id = as.character(start_station_id) ,
                   end_station_id = as.character(end_station_id))
trips_total <- bind_rows(may_2020, jun_2020, jul_2020, aug_2020,
                         sep_2020, oct_2020, nov_2020, dec_2020,
                         jan_2021, feb_2021, mar_2021, apr_2021)

trips_total <- trips_total %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))
save(trips_total, file = "trips_total.RData")
trips_total$date <- as.Date(trips_total$started_at)
trips_total$month <- format(as.Date(trips_total$date), "%m")
trips_total$day <- format(as.Date(trips_total$date), "%d")
trips_total$year <- format(as.Date(trips_total$date), "%Y")
trips_total$day_of_week <- format(as.Date(trips_total$date), "%A")
trips_total$ride_length <- difftime(trips_total$ended_at,trips_total$started_at)
is.factor(trips_total$ride_length)
trips_total$ride_length <- as.numeric(as.character(trips_total$ride_length))
is.numeric(trips_total$ride_length)
str(trips_total)
trips_total$ride_length <- format(as.numeric(difftime(trips_total$ended_at,trips_total$started_at, units = ("mins"))), digits = 1, nsmall = 1)
is.factor(trips_total$ride_length)
trips_total$ride_length <- as.numeric(as.character(trips_total$ride_length))
is.numeric(trips_total$ride_length)
str(trips_total)
trips_total_filtered <- trips_total[!(trips_total$start_station_name == "HQ QR" | trips_total$ride_length<1 | trips_total$ride_length>1440),]
trips_total_filtered_na = na.omit(trips_total_filtered)
trips_total_filtered_na <- trips_total_filtered_na %>% distinct()
save(trips_total_filtered_na, file = "trips_total_filtered_na.RData")
aggregate(trips_total_filtered_na$ride_length ~ trips_total_filtered_na$member_casual, FUN = mean)
aggregate(trips_total_filtered_na$ride_length ~ trips_total_filtered_na$member_casual, FUN = median)
trips_total_filtered_na$day_of_week <- ordered(trips_total_filtered_na$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
trips_total_filtered_na %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()                           #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%      # calculates the average duration
  arrange(member_casual, weekday)  
trips_total_filtered_na %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
trips_total_filtered_na %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
write.csv(trips_total_filtered_na,"Cleaned_Cyclistic_Dataframe_v1.csv")









