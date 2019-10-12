library(tidyverse)

yellow.jan.df <-
  read_csv(
    "Taxi/Data/yellow_tripdata_2019-01.csv",
    col_types = cols(
      RatecodeID = col_integer(),
      VendorID = col_integer(),
      passenger_count = col_integer(),
      tpep_dropoff_datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      tpep_pickup_datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      congestion_surcharge = col_double()
    )
  )


DescTools::Winsorize(yellow.jan.df$total_amount, probs = c(0.05, .90)) %>% hist()

scales::comma(sum(yellow.jan.df$total_amount))

nyc.neighborhoods <- rgdal::readOGR("~/Dropbox/Data/Projects/NYC-data/Taxi/Taxi-zones/taxi_zones.shp", 'OGRGeoJSON', verbose = FALSE)
summary(nyc.neighborhoods)
nyc.df <- broom::tidy(nyc.neighborhoods)

