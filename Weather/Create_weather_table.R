library(tidyverse)
library(lubridate)
library(RSQLite)

# this script creates a table of the daily weather in Central Park
#  The table is saved to the database and can be joined to other tables
#  by matching on Date

# helper functions --------------------------------------------------------

# function to convert text to proper case
toproper <- function(name) paste0(toupper(substr(name, 1, 1)), tolower(substring(name, 2)))


# build the dataset and save to CSV -----------------------------------------
# 
# # download the data. This data was the result of a query on https://www.ncdc.noaa.gov/cdo-web/search
# CP.weather.df <- read_table("https://www.ncei.noaa.gov/orders/cdo/1954099.txt")
# 
# # remove first row. It's just underscores
# CP.weather.df <- CP.weather.df[-1,]
# 
# # fix the column names
# colnames(CP.weather.df)[1:6] <- toproper(colnames(CP.weather.df)[1:6])
# 
# # fix the date
# CP.weather.df$Date <- as_date(CP.weather.df$Date)
# 
# # which dates have valid readings for each field
# has.value <- function(col){
#   min.date <- min(CP.weather.df$Date[CP.weather.df[,col] != -9999])
#   max.date <- max(CP.weather.df$Date[CP.weather.df[,col] != -9999])
#   return(c(min.date, max.date))
# }
# 
# minmax.dates <- lapply(colnames(CP.weather.df), has.value)
# names(minmax.dates) <- colnames(CP.weather.df)
# minmax.dates <- t(as_tibble(minmax.dates))
# colnames(minmax.dates) <- c("Min.date", "Max.date")
# print(minmax.dates)
# print(minmax.dates[grep("2019-*", minmax.dates[,2]),])
# cols.to.keep <- rownames(minmax.dates[grep("2019-*", minmax.dates[,2]),])
# 
# # keep only the columns that are important
# cols.to.keep <- cols.to.keep[cols.to.keep %in% c("Date", "PRCP", "SNWD", "SNOW", "TMAX", "TMIN", "AWND", "WSF2")]
# CP.weather.df <- CP.weather.df[, cols.to.keep]
# colnames(CP.weather.df) <- c("Date", "Precipation", "Snow.depth", "Snowfall", "Max.temp", "Min.temp", "Avg.wind.speed", "Gust.speed")
# rm(minmax.dates, cols.to.keep, has.value)
# 
# # filter out dates before 2000
# CP.weather.df <- filter(CP.weather.df, Date >= as_date("2000-01-01"))
# 
# # drop the last row
# CP.weather.df <- CP.weather.df[1:(nrow(CP.weather.df)-1),]
# 
# # replace -9999 with NAs
# CP.weather.df <- lapply(CP.weather.df, function(col){
#   col[col == -9999] <- NA
#   col
# }) %>% as_tibble()
# 
# # save the table
# write_csv(CP.weather.df, "Weather/Data/CP.weather.df.csv")
# 
# rm(CP.weather.df)


# read in the CSV and write to the database --------------------------

# read in the csv 
CP.weather.df <- read_csv("Weather/Data/CP.weather.df.csv")

# establish the connection to the database
conn <- dbConnect(RSQLite::SQLite(), "NYC.db")

# add the table to the database
dbWriteTable(
  conn = conn,
  name = "Central.Park.weather",
  value = CP.weather.df,
  field.types = c(
    Date = "int",
    Precipation = "real",
    Snow.depth = "real",
    Snowfall = "real",
    Max.temp = "int",
    Min.temp = "int",
    Avg.wind.speed = "real",
    Gust.speed = "real"
  )
)

# list tables
dbListTables(conn)

# test a query
tbl(conn, "Central.Park.weather") %>%
  collect() %>%
  mutate(Date = as_date(Date))




