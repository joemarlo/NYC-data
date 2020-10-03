library(tidyverse)
library(hms)
library(lubridate)
library(RSQLite)
source('Plots/ggplot-theme.R')


# connect to database and read in data to memory --------------------------

# establish the connection to the database
conn <- dbConnect(RSQLite::SQLite(), "NYC.db")

# df of all trips
master_df <- dbListTables(conn) %>% 
  grep("citibike*", ., value = T) %>% 
  map_dfr(., function(year_table){
    tbl(conn, year_table) %>% 
      collect()
  })

# sample the dataset for performance
master_df <- slice_sample(master_df, n = 5000000)

# remove stations without lat long 
# and service stations
# and NAs (mostly birth year missing)
master_df <- master_df %>%
  filter(
    Start.station.latitude != 0,
    Start.station.longitude != 0,
    End.station.latitude != 0,
    End.station.longitude != 0,
    Start.station.id != 3488,
    End.station.id != 3488,
    Start.station.id != 3650,
    End.station.id != 3650
  ) %>% 
  na.omit()

# unique stations
stations <- master_df %>% 
  distinct(Start.station.id, Start.station.latitude, Start.station.longitude) %>% 
  group_by(Start.station.id) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# subway stations
subway_stations <- tbl(conn, "station.lat.long") %>% collect()

# km between two lat long points
# lat 1 degree = 110.574 km
# long 1 degree = 111.320*cos(latitude) km
# https://stackoverflow.com/questions/1253499/simple-calculations-for-working-with-lat-lon-and-km-distance
stations$dist_to_subway <- map2_dbl(.x = stations$Start.station.latitude,
         .y = stations$Start.station.longitude,
         .f = function(lat1, long1){
  lat_dist <- subway_stations$Lat - lat1
  long_dist <- subway_stations$Long - long1
  long_dist <- long_dist * cos((lat1 * pi) / (180))
  dist <- sqrt(lat_dist^2 + long_dist^2)
  dist <- dist[dist != 0]
  min(dist)
})

# add distance back to df
master_df <- master_df %>% 
  left_join(stations %>% select(Start.station.id, Start_dist_to_subway = dist_to_subway)) %>% 
  left_join(stations %>% select(End.station.id = Start.station.id, End_dist_to_subway = dist_to_subway)) %>% 
  na.omit() %>% 
  select(Tripduration, Starttime, Stoptime, Start.station.id, End.station.id, 
         Usertype, Birth.year, Gender, Start_dist_to_subway, End_dist_to_subway)

# check final size
dim(master_df)

# write out the dataset
write_csv(master_df, 'Analyses/Citi-Bike-prediction/Citi-Bike-prediction/Citi_Bike_trips/Data.csv')
