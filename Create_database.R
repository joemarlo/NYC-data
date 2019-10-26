library(tidyverse)
library(RSQLite)

# this script takes the downloaded data and builds a single database from it
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
read_subway_files <- function(year) {
  #year should be last two digits; e.g. 19 for 2019
  stopifnot(is.character(year), nchar(year) == 2)
  
  filenames <- list.files(
    path = "Subway-turnstiles/Data/",
    pattern = paste0("turnstile_",
                     as.character(year),
                     ".....txt"),
    all.files = FALSE,
    full.names = FALSE,
    recursive = FALSE,
    ignore.case = FALSE
  )
  
  # read in all those files and combine into one data.frame
  map_df(filenames, function(filename) {
    NameDF <-
      read_csv(
        file = paste0("Subway-turnstiles/Data/", filename),
        col_types = cols(
          `C/A` = col_character(),
          DATE = col_date(format = "%m/%d/%Y"),
          DESC = col_character(),
          DIVISION = col_character(),
          ENTRIES = col_double(),
          EXITS = col_double(),
          SCP = col_character(),
          STATION = col_character(),
          TIME = col_time(format = "%H:%M:%S"),
          UNIT = col_character()
        )
      )
    # add identifier
    NameDF$Source.file <- filename
    return(NameDF)
  })
}

# read in the turnstile files to form the table and append subsequent files. 
# final result is a database with tables for each year
years.to.import <- 2014:2019
sapply(years.to.import, function(year) {
  # read in the files
  df <- read_subway_files(year = substr(as.character(year), 3, 4))
  
  # clean up the column names
  names(df) <- sapply(names(df), toproper)
  names(df)[1] <- "Booth"
  names(df)[3] <- "SCP"
  
  table.name <- paste0("turnstile.", year)
  
  # create table if it doesn't exist
  if (!dbExistsTable(conn, table.name)) {
    dbWriteTable(
      conn = conn,
      name = table.name,
      value = df,
      overwrite = TRUE,
      field.types = c(
        Booth = "text",
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
        Source.file = "text"
      )
    )
  } else {
    # append the file to the database if the database already exists
    dbWriteTable(
      conn = conn,
      name = table.name,
      value = df,
      append = TRUE
    )
  }
})

# list all the tables available in the database
dbListTables(conn)

# test a query
tbl(conn, "turnstile.2014") %>%
  group_by(Source.file) %>%
  summarize(n.rows = n())

# read in the Citi bike data ---------------------------------------------------------------

#function to read in the data, format, and then combine to one dataframe
read_citibike_files <- function(year) {
  #year should be four digits; e.g. 2019
  stopifnot(nchar(year) == 4)
  
  filenames <- list.files(path = "Citi-bike/Data/",
                          pattern = paste0("^", 2019, ".*csv$"))
  
  # read in all those files and combine into one data.frame
  map_df(filenames, function(filename) {
    NameDF <-
      read_csv(
        file = paste0("Citi-bike/Data/", filename),
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
    
    # add identifier
    NameDF$Source.file <- filename
    return(NameDF)
  })
}

# fix the date format in three files that do not match
fix_date <- function(filename) {
  # read in file with correct format
  df <- read_csv(
    file = paste0("Citi-bike/Data/", filename),
    col_types = cols(
      `birth year` = col_double(),
      `end station id` = col_double(),
      `start station id` = col_double(),
      `end station latitude` = col_double(),
      `end station longitude` = col_double(),
      `start station latitude` = col_double(),
      `start station longitude` = col_double(),
      starttime = col_datetime(format = "%m/%d/%Y %H:%M:%S"),
      stoptime = col_datetime(format = "%m/%d/%Y %H:%M:%S"),
      tripduration = col_integer()
    )
  )
  
  # write the file back to to the same spot 
  write_csv(df, path = paste0("Citi-bike/Data/", filename))
}

files <- c("201409-citibike-tripdata.csv",
           "201410-citibike-tripdata.csv",
           "201411-citibike-tripdata.csv",
           "201412-citibike-tripdata.csv")
lapply(files, fix_date)
rm(files, fix_date)

# read in the citibike files by month, cleanup, then append to the database
# final result is a database with tables for each year
years.to.import <- 2013:2019
sapply(years.to.import, function(year) {
  # read in the files
  df <- read_citibike_files(year = year)
  
  # clean up the data
  names(df) <- str_replace_all(names(df), " ", ".")
  names(df) <- sapply(names(df), toproper)
  df$Gender <- factor(
    df$Gender,
    levels = c(0, 1, 2),
    labels = c("Unknown", "Male", "Female")
  )
  
  # set the table name by year
  table.name <- paste0("citibike.", year)
  
  # create table if it doesn't exist
  if (!dbExistsTable(conn, table.name)) {
    dbWriteTable(
      conn = conn,
      name = table.name,
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
        Gender = "text",
        Source.file = "text"
      )
    )
  } else{
    # append the file to the database if the database already exists
    dbWriteTable(
      conn = conn,
      name = table.name,
      value = df,
      append = TRUE
    )
  }
})

# list all the tables available in the database
dbListTables(conn)

# test a query
tbl(conn, "citibike.2019") %>%
  group_by(Source.file) %>%
  summarize(n.rows = n())

# remove all tables from the database
# sapply(dbListTables(conn), function(table) dbRemoveTable(conn, table))
