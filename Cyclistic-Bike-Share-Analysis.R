

library(tidyverse)
library(lubridate)

# setwd()
setwd("~/Google-Date-Analytics-Course/Case-Study-Track-1/Cyclistic-trip-data/Trip-History")

# Data Collection

# Load DataSets

apr20 <- read_csv("202004-divvy-tripdata.csv")
may20 <- read_csv("202005-divvy-tripdata.csv")
jun20 <- read_csv("202006-divvy-tripdata.csv")
jul20 <- read_csv("202007-divvy-tripdata.csv")
aug20 <- read_csv("202008-divvy-tripdata.csv")
sep20 <- read_csv("202009-divvy-tripdata.csv")
oct20 <- read_csv("202010-divvy-tripdata.csv")
nov20 <- read_csv("202011-divvy-tripdata.csv")
dec20 <- read_csv("202012-divvy-tripdata.csv")
jan21 <- read_csv("202101-divvy-tripdata.csv")
feb21 <- read_csv("202102-divvy-tripdata.csv")
mar21 <- read_csv("202103-divvy-tripdata.csv")

# Data Wrangling

# Check for consistency in column names
colnames(apr20)
colnames(may20)
colnames(jun20)
colnames(jul20)
colnames(aug20)
colnames(sep20)
colnames(oct20)
colnames(nov20)
colnames(dec20)
colnames(jan21)
colnames(feb21)
colnames(mar21)

# Check structure of datasets for inconsistencies
str(apr20)
str(may20)
str(jun20)
str(jul20)
str(aug20)
str(sep20)
str(oct20)
str(nov20)
str(dec20)
str(jan21)
str(feb21)
str(mar21)


# Change data types from 'dec20' to 'mar21' in 'start_station_id' and 'end_station_id' columns from 'char' to 'double'.

dec20 <-  mutate(dec20, start_station_id = as.double(start_station_id),
                 end_station_id = as.double(end_station_id))
jan21 <-  mutate(jan21, start_station_id = as.double(start_station_id),
                 end_station_id = as.double(end_station_id))
feb21 <-  mutate(feb21, start_station_id = as.double(start_station_id),
                 end_station_id = as.double(end_station_id))
mar21 <-  mutate(mar21, start_station_id = as.double(start_station_id),
                 end_station_id = as.double(end_station_id))

# Confirm changes took effect

is.double(dec20$start_station_id)
is.double(dec20$end_station_id)
is.double(jan21$start_station_id)
is.double(jan21$end_station_id)
is.double(feb21$start_station_id)
is.double(feb21$end_station_id)
is.double(mar21$start_station_id)
is.double(mar21$end_station_id)

# Combine all data sets into one data frame

all_trips <- bind_rows(apr20, may20, jun20, jul20, aug20, sep20, oct20, nov20, dec20, jan21, feb21, mar21)


# cleaning the data

glimpse(all_trips)
summary(all_trips)


# Remove columns not needed
all_trips <- all_trips %>%
select(-c(start_lat:end_lng))
glimpse(all_trips)


# Rename some columns

all_trips <- all_trips %>%
rename(ride_type = rideable_type, 
      start_time = started_at,
      end_time = ended_at,
      customer_type = member_casual)
glimpse(all_trips)

# Add new columns for aggregate functions

# column for day of the week for each trip
all_trips$day_of_the_week <- format(as.Date(all_trips$start_time),'%a')

# column for day of the week 
all_trips$month <- format(as.Date(all_trips$start_time),'%b_%y')

# column for time of the day each trip started

all_trips$time <- format(all_trips$start_time, format = "%H:%M")
all_trips$time <- as.POSIXct(all_trips$time, format = "%H:%M")

# column for trip duration in min
all_trips$trip_duration <- (as.double(difftime(all_trips$end_time, all_trips$start_time)))/60

# check the data frame
glimpse(all_trips)

# check for trip lengths less than 0
nrow(subset(all_trips,trip_duration < 0))


# Drop all NA:
all_trips_v2 <- drop_na(all_trips)


# remove negative trip duration 
all_trips_v2 <- all_trips[!(all_trips$trip_duration < 0),]

# remove test rides
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR"),]


# Check data frame
glimpse(all_trips_v2)

# checking count of distinct values
table(all_trips_v2$customer_type)

# Aggregating total trip duration by customer type
setNames(aggregate(trip_duration ~ customer_type, all_trips_v2, sum), c("customer_type", "total_trip_duration(mins)"))


# Descriptive Analysis

# summary of trip_duration for all trips
summary(all_trips_v2$trip_duration)

# summary of trip_duration by customer_type
all_trips_v2 %>%
  group_by(customer_type) %>%
  summarise(min_trip_duration = min(trip_duration),max_trip_duration = max(trip_duration),
            median_trip_duration = median(trip_duration), mean_trip_duration = mean(trip_duration))

# Total number of trips by customer type and day of the week

# fix the order for the day_of_the_week and month
all_trips_v2$day_of_the_week <- ordered(all_trips_v2$day_of_the_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
all_trips_v2$month <- ordered(all_trips_v2$month, levels=c("Apr_20", "May_20", "Jun_20", "Jul_20", "Aug_20", "Sep_20", "Oct_20",
                                                           "Nov_20", "Dec_20", "Jan_21", "Feb_21", "Mar_21"))
all_trips_v2 %>% 
  group_by(customer_type, day_of_the_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(trip_duration)) %>% 
  arrange(customer_type, desc(number_of_rides))

# Average number of trips by customer type and month

all_trips_v2 %>% 
  group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(trip_duration)) %>% 
  arrange(customer_type,desc(number_of_rides))
 


# Visualizations


# Total number of trips by customer type and day of the week:

all_trips_v2 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, day_of_the_week)  %>% 
  ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


# Total number of trips by customer type and month

  all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  

    
# Creating a csv file of the clean data for more visualizations in Tableau.
   
     clean_data <- aggregate(all_trips_v2$trip_duration ~ all_trips_v2$customer_type + all_trips_v2$day_of_the_week, FUN = mean)
    write.csv(clean_data, "Clean Data.csv", row.names = F)
    

    