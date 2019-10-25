library(tidyverse)
library(RSQLite)

# this script takes the downloaded data and builds a single database out of it
# must run download_instruction.sh shell scripts to download the data
#  before running this script


# connect to database -----------------------------------------------------

# connect to database NYC.db; if it doesn't exist this will
#  create it in the working directory
conn <- dbConnect(RSQLite::SQLite(), "NYC.db")


# helper functions --------------------------------------------------------

# function to convert text to proper case
toproper <- function(name) paste0(toupper(substr(name, 1, 1)), tolower(substring(name, 2)))


# read in turnstile data --------------------------------------------------

#function to read in the data, format, and then combine to one dataframe
read_subway_files<- function(year, month){
  #year should be last two digits; e.g. 19 for 2019
  #month should be two digits; e.g. 01 for January
  
  filenames <- list.files(path = "Subway-turnstiles/Data/",
                          pattern = paste0("turnstile_",
                                           as.character(year),
                                           as.character(month),
                                           "...txt"),
                          all.files = FALSE, full.names = FALSE,
                          recursive = FALSE, ignore.case = FALSE)
  
  map_df(filenames, function(filename){
    NameDF <- read_csv(file = paste0("Subway-turnstiles/Data/", filename),
                       col_types = cols(
                         DATE = col_date(format = "%m/%d/%Y"),
                         ENTRIES = col_double(),
                         EXITS = col_double(),
                         TIME = col_time(format = "%H:%M:%S")
                       ))
    NameDF$Source.file <- filename
    return(NameDF)
  })
}

# read in the turnstile files, combine by month, and then write a single file
#   per month to the database
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")
sapply(months, function(month) {
  
  # read in the file
  df <- read_subway_files(year = "19", month = month)
  
  # clean up the column names
  names(df) <- sapply(names(df), toproper)
  names(df)[1] <- "Booth"
  names(df)[3] <- "SCP"
  
  # write the file to the database
  dbWriteTable(conn = conn,
               name = paste0("turnstile.2019.", month),
               value = df,
               overwrite = TRUE,
               field.types = c(Booth = "text",
                               Unit = "text",
                               SCP = "text",
                               Station = "text",
                               Linename = "text",
                               Division = "text",
                               Date = "int",
                               Time = "int",
                               Desc = "text",
                               Entries = "real",
                               Exits = "real",
                               Source.file = "text"))
})

# list all the tables available in the database
dbListTables(conn)

# test that a query works
tmp <- tbl(conn, "turnstile.2019.01")
tmp %>%
  select(UNIT, SCP, STATION, TIME, ENTRIES, EXITS) %>%
  group_by(STATION) %>%
  summarize(Entries = sum(ENTRIES),
            Exits = sum(EXITS))


# read in the Citi bike data ---------------------------------------------------------------

# read in the turnstile files by month, cleanup then file to the database
#  file output is one file per month
years <- c(2018, 2019)
sapply(years, function(year) {
  fileNames <- list.files(path = "Citi-bike/Data/",
                          pattern = paste0(year, "[0-1][1-9]-citibike-tripdata.csv"))
  
  lapply(fileNames, function(fileName) {
    # read in a singel month of data and format it
    df <- read_csv(
      paste("Citi-bike/Data", fileName, sep = "/"),
      col_types = cols(
        `birth year` = col_double(),
        `end station id` = col_double(),
        `start station id` = col_double(),
        `end station latitude` = col_double(),
        `end station longitude` = col_double(),
        `start station latitude` = col_double(),
        `start station longitude` = col_double(),
        starttime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
        stoptime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
        tripduration = col_integer()
      )
    )
    
    # get the month so it can be used for the name
    month <- substr(fileName, 5, 6)
    
    # clean up the data
    names(df) <- str_replace_all(names(df), " ", ".")
    names(df) <- sapply(names(df), toproper)
    df$Gender <- factor(df$Gender,
                        levels = c(0, 1, 2),
                        labels = c("Unknown", "Male", "Female"))
    
    # write the file to the database
    dbWriteTable(
      conn = conn,
      name = paste0("citibike.", year, ".", month),
      value = df,
      overwrite = TRUE,
      field.types = c(
        Tripduration = "int",
        Starttime = "datetime",
        Stoptime = "datetime",
        Start.station.id = "int",
        Start.station.name = "text",
        Start.station.latitude = "real",
        Start.station.longitude = "real",
        End.station.id = "int",
        End.station.name = "text",
        End.station.latitude = "real",
        End.station.longitude = "real",
        Bikeid = "int",
        Usertype = "text",
        Birth.year = "int",
        Gender = "text")
      )
    })
})


