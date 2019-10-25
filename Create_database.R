library(tidyverse)
library(RSQLite)

# this script takes the downloaded data and builds a single database out of it
# must run download_instruction.sh shell scripts to download the data
#  before running this script


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
  df <- read_subway_files(year = "19", month = month) %>% bind_rows()
  
  # clean up the column names
  names(df) <- sapply(names(turnstile.df), toproper)
  names(df)[1] <- "Booth"
  names(df)[3] <- "SCP"
  
  # write the file to the database
  dbWriteTable(conn = conn,
               name = paste0("turnstile.2019.", month),
               value = df,
               overwrite = TRUE,
               field.types = c(Booth = "TEXT",
                               Unit = "TEXT",
                               SCP = "TEXT",
                               Station = "TEXT",
                               Linename = "TEXT",
                               Division = "TEXT",
                               Date = "INTEGER",
                               Time = "INTEGER",
                               Desc = "TEXT",
                               Entries = "REAL",
                               Exits = "REAL",
                               Source.file = "TEXT"))
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


