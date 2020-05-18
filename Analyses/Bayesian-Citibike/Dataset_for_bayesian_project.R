library(tidyverse)
library(hms)
library(lubridate)
library(RSQLite)
source('Plots/ggplot-theme.R')


# connect to database and read in data to memory --------------------------

# establish the connection to the database
conn <- dbConnect(RSQLite::SQLite(), "NYC.db")

# read in all of 2019 data
bike.trips.df <- tbl(conn, "citibike.2019") %>%
    collect() %>%
    mutate(
      Starttime = as_datetime(Starttime),
      Stoptime = as_datetime(Stoptime),
      Gender = factor(Gender, levels = c("Unknown", "Male", "Female"))
    )


# individual trip duration ------------------------------------------------

sampled_data <- sample_n(bike.trips.df, 50000) %>% 
  mutate(Age = 2019 - Birth.year,
         Date = as.Date(Starttime),
         Commuting_hours = hour(Starttime) %in% c(6:10, 16:19),
         Weekday = weekdays(Date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
         Subscriber = Usertype == 'Subscriber',
         Male = Gender == 'Male') %>%
  select(Trip_duration_seconds = Tripduration, Date, Commuting_hours, Weekday, Subscriber, Age, Male)

# filter outliers
outlier_bounds <- quantile(sampled_data$Trip_duration_seconds, c(0.025, 0.975))
sampled_data <- sampled_data %>% 
  filter(Trip_duration_seconds >= outlier_bounds[[1]],
         Trip_duration_seconds <= outlier_bounds[[2]])


# count of trips by date --------------------------------------------------------------

# function to count number of trips by date
get_daily_counts <- function(year){
  tbl(conn, paste0("citibike.", year)) %>%
    select(Starttime) %>%
    collect() %>% 
    mutate(Date = as.Date(as_datetime(Starttime))) %>% 
    select(Date) %>% 
    count(Date)
}

# count trips by date for 2017:2019
daily_counts <- map_dfr(2017:2019, get_daily_counts)

# add weekday variable
daily_counts <- daily_counts %>% 
  rename(Trip_count = n) %>% 
  mutate(Weekday = weekdays(Date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
         
# daily weather
weather <- tbl(conn, "Central.Park.weather") %>% 
  select(Date, Precipitation, Temp = Max.temp, Gust_speed = Gust.speed) %>% 
  collect() %>%
  mutate(Date = as_date(Date))

# merge the two datasets and turn weekday and precip into booleans
final_dataset <- daily_counts %>% 
  left_join(weather, by = "Date") %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(Weekday = as.numeric(Weekday),
         Precipitation = as.numeric(Precipitation > 0.20)) %>% 
  select(-Date)

# write out the dataset
write_csv(final_dataset, 'Analyses/Bayesian-Citibike/Daily_Citibike_counts.csv')
