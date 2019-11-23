library(tidyverse)
library(RSQLite)

# this script takes the downloaded data and builds a single SQLite database from it
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
    # append the file to the table if the table already exists
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

# the following files have different date formats and need to be accounted for a case-when statement

# these files have dateformat of "%m/%d/%Y %H:%M"
date.format.1 <- c(
  "20150[1-3]-citibike-tripdata.csv",
  "201506-citibike-tripdata.csv"
)

# these files have dateformat of "%m/%d/%Y %H:%M:%S"
date.format.2 <- c(
  "201409-citibike-tripdata.csv",
  "20141[0-2]-citibike-tripdata.csv",
  "20150[4-5]-citibike-tripdata.csv",
  "20150[7-9]-citibike-tripdata.csv",
  "20151[0-2]-citibike-tripdata.csv",
  "20160[1-9]-citibike-tripdata.csv"
)


#function to read in the data, format, and then combine to one dataframe
read_citibike_files <- function(year) {
  
  #year should be four digits; e.g. 2019
  stopifnot(nchar(year) == 4)
  
  # list all the files in the directory that match this year
  filenames <- list.files(path = "Citi-bike/Data/",
                          pattern = paste0("^", year, ".*csv$"))
  
  # read in all those files and combine into one data.frame
  map_df(filenames, function(filename) {
    # some files have different date formats and need to be adjusted
    # date.format.# is a list of file names (regex patterns) that the filename
    #  can match to. If it does match, then it returns a new date.format
    date.format <-
      case_when(
        any(sapply(date.format.1, function(pattern) grepl(pattern, filename))) ~ "%m/%d/%Y %H:%M",
        any(sapply(date.format.2, function(pattern) grepl(pattern, filename))) ~ "%m/%d/%Y %H:%M:%S",
        TRUE ~ "%Y-%m-%d %H:%M:%S"
      )
    
    # read in the data but for certain pre-determined files that have inconsistent column names
    if (grepl("20161[0-2]-citibike-tripdata.csv", filename) |
        grepl("20170[1-3]-citibike-tripdata.csv", filename)) {
      # if the filename name is in that list then use a different set of column names
      NameDF <-
        read_csv(
          file = paste0("Citi-bike/Data/", filename),
          col_types = cols(
            `Birth Year` = col_double(),
            `Start Station ID` = col_double(),
            `End Station ID` = col_double(),
            `Start Station Name` = col_character(),
            `End Station Name` = col_character(),
            `Start Station Latitude` = col_double(),
            `End Station Latitude` = col_double(),
            `Start Station Longitude` = col_double(),
            `End Station Longitude` = col_double(),
            `Bike ID` = col_double(),
            `User Type` = col_character(),
            `Start Time` = col_datetime(format = date.format),
            `Stop Time` = col_datetime(format = date.format),
            `Trip Duration` = col_integer(),
            Gender = col_double()
          )
        )
      
      # need to change names to the more common convention so columns aren't duplicated
      #  note that the order here must match the column order
      colnames(NameDF) <- c(
        "tripduration",
        "starttime",
        "stoptime",
        "start station id",
        "start station name",
        "start station latitude",
        "start station longitude",
        "end station id",
        "end station name",
        "end station latitude",
        "end station longitude",
        "bikeid",
        "usertype",
        "birth year",
        "gender"
      )
    } else {
      # for all other files, use the default names
      NameDF <-
        read_csv(
          file = paste0("Citi-bike/Data/", filename),
          col_types = cols(
            `birth year` = col_double(),
            `start station id` = col_double(),
            `end station id` = col_double(),
            `start station name` = col_character(),
            `end station name` = col_character(),
            `start station latitude` = col_double(),
            `end station latitude` = col_double(),
            `start station longitude` = col_double(),
            `end station longitude` = col_double(),
            bikeid = col_double(),
            usertype = col_character(),
            starttime = col_datetime(format = date.format),
            stoptime = col_datetime(format = date.format),
            tripduration = col_integer(),
            gender = col_double()
          )
        )
    }
    
    # add identifier
    NameDF$Source.file <- filename
    return(NameDF)
  })
}


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
        Starttime = "int",
        Stoptime = "int",
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
    # append the file to the table if the table already exists
    dbWriteTable(
      conn = conn,
      name = table.name,
      value = df,
      append = TRUE
    )
  }
})

# check to see if any tables have issues with the starttime column
dbListTables(conn) %>%
  grep("citibike*", ., value = T) %>%
  sapply(., function(table) {
    tbl(conn, table) %>%
      filter(is.na(starttime)) %>%
      collect() %>%
      summarize(n())
  })

# check number of rows per table
dbListTables(conn) %>%
  grep("citibike*", ., value = T) %>%
  sapply(., function(table) {
    tbl(conn, table) %>%
      tally() %>%
      collect() %>%
      as.numeric()
  })

# list all the tables available in the database
dbListTables(conn)

# test a query
tbl(conn, "citibike.2019") %>%
  group_by(Source.file) %>%
  summarize(n.rows = n())

# remove all tables from the database
# sapply(dbListTables(conn), function(table) dbRemoveTable(conn, table))

# remove citibike tables
# dbListTables(conn) %>% grep("citi", ., value = T) %>% sapply(., function(table) dbRemoveTable(conn, table))

