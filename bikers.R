# Install the tidyverse package
install.packages("tidyverse")

# Load the tidyverse package
# Loading the tidyverse automatically loads the dplyr, 
# readr, and lubridate packages
# Load knitr too
library(tidyverse)
library(knitr)

# Verify that column names match before merging the CSV files
jan_2022 <- read.csv('2022-divvy-tripdata/202201-divvy-tripdata.csv')
feb_2022 <- read.csv('2022-divvy-tripdata/202202-divvy-tripdata.csv')
mar_2022 <- read.csv('2022-divvy-tripdata/202203-divvy-tripdata.csv')
colnames(jan_2022)
colnames(feb_2022)
colnames(mar_2022)

# Remove data frames you no longer need
rm(jan_2022, feb_2022, mar_2022)

# Merge all CSV files containing the datasets into one
cyclistic <- list.files(path='2022-divvy-tripdata', full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

# Inspect the data frame
head(cyclistic)
str(cyclistic)

# Check for columns with missing values
cbind(
  lapply(
    lapply(cyclistic, is.na), sum
  )
)

# Check for bad data
# 1. Confirm how many different bike types there are
table(cyclistic$rideable_type)

# 2. Check if there are times when ended_at occurs 
# earlier than started_at
filter(cyclistic, ended_at < started_at)

# 3. Check for times when ended_at == started_at
filter(cyclistic, ended_at == started_at)

# Remove those instances of bad data 
# where the time the trip ended was earlier than when it started
cyclistic_v02 <- filter(cyclistic, ended_at >= started_at)

# 4. See the different categories of riders
table(cyclistic_v02$member_casual)

# Add three new columns 
# 1. ride_length, defined as the duration of the ride, formatted in HH:MM:SS
# 2. day_of_week: the day each ride started
# 3. month_of_year: the month the ride took place

# Rename the categories of riders in the `member_casual` column.
cyclistic_v02$member_casual[cyclistic_v02$member_casual == "casual"] <- 
  "Casual Rider"
cyclistic_v02$member_casual[cyclistic_v02$member_casual == "member"] <- 
  "Annual Member"

# Generate csv on number of riders per group
write_csv(data.frame(
  table(cyclistic_v02$member_casual)
),
file = "riders.csv")

# Do descriptive statistics for each rider type
aggregate(ride_length ~ member_casual, data = cyclistic_v02, summary)
summary_stat <- data.frame(
  Rider = c("Casual Rider", "Annual Member"),
  Minimum = c("0S", "0S"),
  Median = c("12M 54S", "7M 46S"),
  Mean = c("31M 20S", "11M 49S"),
  Maximum = c("23d 20H 34M 4S", "1d 1H 59M 54S")
)
kable(summary_stat, caption = "Descriptive statistics for each rider type")

# Create table on average ride duration per day                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
avg_ride_duration_per_day <- 
  aggregate(
    as.numeric(ride_length)/60 ~ member_casual + day_of_week, 
    data = cyclistic_v02, 
    mean
  )

avg_ride_duration_per_day <- rename(avg_ride_duration_per_day, 
                                    Rider = member_casual,
                                    `Day of Week` = day_of_week,
                                    `Mean (in minutes)` = `as.numeric(ride_length)/60`)

kable(avg_ride_duration_per_day, caption = "Daily average trip duration")

# Create visualization on average ride duration per day
cyclistic_v02 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(as.numeric(ride_length))/60) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "stack") +
  labs(title = "Daily Average Trip Duration of City of Chicago Bikers",
       subtitle = "Casual Riders vs Annual Members",
       caption = "Data Source: Motivate Int'l Inc",
       fill = "Riders") +
  xlab("Day of the Week") +
  ylab("Minutes")

# Create visualization on bicycle types used for the trips
cyclistic_v02 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("blue", "purple")) +
  theme_minimal() +
  labs(title = "Bicycle Used During Trips",
       subtitle = "Casual Riders vs Annual Members",
       caption = "Data Source: Motivate Int'l Inc",
       fill = "Riders") +
  xlab("Bicycle") +
  ylab("Count")

# Create visualization on number of rides per month
cyclistic_v02 %>% 
  group_by(member_casual, month_of_year) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = month_of_year, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_grey() +
  theme_minimal() +
  labs(title = "Monthly Number of Rides Among City of Chicago Bikers",
       subtitle = "Casual Riders vs Annual Members",
       caption = "Data Source: Motivate Int'l Inc",
       fill = "Riders") +
  xlab("Month") +
  ylab("Ride")