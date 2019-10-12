library(tidyverse)
library(httr)
library(hms)
library(DescTools)
library(stringdist)
library(parallel)

cpu.cores <- detectCores()
#there are some major data cleaning issues

turnstile_190928 <- read_csv("/home/joemarlo/Dropbox/Data/Projects/Subway-turnstiles/turnstile_190928.txt",
                             col_types = cols(DATE = col_date(format = "%m/%d/%Y"), 
                                              ENTRIES = col_double(),
                                              EXITS = col_double(), 
                                              TIME = col_time(format = "%H:%M:%S")))
#double check if conversion of ENTRIES and EXITS to double is correct

#import list of stations and tidy | need this for lat long
stations_df <- GET("https://data.cityofnewyork.us/api/views/kk4q-3rt2/rows.csv?accessType=DOWNLOAD")
stations_df <- content(stations_df)

#seperate out the lat/long
stations_df <- stations_df %>%
  mutate(the_geom = str_remove(the_geom, "POINT [(]") %>% str_remove(., "[)]")) %>%
  separate(the_geom, into = c("Lat", "Long"), sep = " ") %>%
  mutate(Lat = as.double(Lat),
         Long = as.double(Long)) %>%
  rename(STATION = NAME) %>%
  select(STATION, Lat, Long)

#check the station names; most station names don't match so need to fuzzy match
unique(turnstile_190928$STATION)
unique(stations_df$STATION)

#convert station names to upper case b/c orig names are upper
stations_df$STATION <- toupper(stations_df$STATION)

#match the original names to the new names using stringsim method
orig_names <- unique(turnstile_190928$STATION)
new_names <- sapply(orig_names, function(x){
  index <- stringsim(x, unique(stations_df$STATION), method = "osa") %>% which.max(.)
  unique(stations_df$STATION)[index]
})

#rerun the previous function with "qgram" and "cosine" as the method; create lookup matching table to see which one works best
name_match_tbl <- tibble(Orig = orig_names, New_osa = new_names) #, New_qgram = new_names_2, New_cosine = new_names_3)
View(name_match_tbl)

#examine where the methods don't agree
name_match_tbl %>%
  filter(!(New_osa == New_qgram & New_osa == New_cosine)) %>%
  View()
#"osa" method seems like the best after manually examining
#a better method would to be not allow duplicates and then maximize average stringsim score.
#   it won't work though b/c stations_df has fewer station names than the turnstile data

#use the osa method to replace names in the turnstile dataset and add lat/long
turnstile_190928$STATION <- name_match_tbl$New_osa[match(turnstile_190928$STATION, name_match_tbl$Orig)]
turnstile_190928 <- left_join(x = turnstile_190928, y = stations_df, by = "STATION")

#view one station at one hour of one day
turnstile_190928 %>%
  filter(STATION == "59TH ST",
         DATE == as.Date("2019-09-21"),
         TIME == as_hms("20:00:00"))


#calculate entries by grouping by time
# entries_exits <- turnstile_190928 %>%
#   group_by(STATION, DATE, TIME) %>%
#   # filter(TIME == as.hms("20:00:00")) %>%
#   # # filter(as.POSIXlt(TIME)$sec == 0,
#   # #        as.POSIXlt(TIME)$min == 0,
#   # #        TIME == max(TIME)) %>%
#   summarize(ENTRIES = sum(ENTRIES),
#             EXITS = sum(EXITS)) %>%
#   group_by(STATION) %>%
#   arrange(STATION, DATE,) %>%
#   mutate(Total_entries = ENTRIES - lag(ENTRIES),
#          Total_exits = EXITS - lag(EXITS)) %>%
#   select(-c(ENTRIES, EXITS)) %>%
#   ungroup()
  
#calculate entries by indexing 
entries_exits <- turnstile_190928 %>%
  filter(TIME %in% as_hms(paste0(seq(2, 20, 2), ":00:00"))) %>% #removes unique times
  arrange(STATION, `C/A`, UNIT, LINENAME, DIVISION, desc(DATE), desc(TIME), desc(ENTRIES), desc(EXITS)) %>%
  group_by(STATION, DATE, TIME) %>%
  mutate(turnstile_ID = row_number(),
         unique_ID = paste(STATION, turnstile_ID, sep = "-")) %>%
  ungroup()

#core problem after this line is outliers register values;
#    have tried winsorize with different intervals, replacing values outside IQR range w/NAs
#    pretty much everything after this line doesn't work


# apply winsorizing at the turnstile level; this doesn't work well
# invisible(mclapply(
#   X = unique(entries_exits$unique_ID),
#   mc.cores = cpu.cores,
#   FUN = function(ID) {
#     entries_exits[entries_exits$unique_ID == ID,]$ENTRIES <<-
#       Winsorize(
#         x = entries_exits[entries_exits$unique_ID == ID,]$ENTRIES,
#         minval = 0,
#         # maxval = 100000,
#         probs = c(0.1, 0.9),
#         na.rm = TRUE
#       )
#   }
# ))

entries_exits2 <- invisible(mclapply(
  X = unique(entries_exits$unique_ID),
  mc.cores = cpu.cores,
  FUN = function(ID) {
    x <- entries_exits[entries_exits$unique_ID == ID, ]$ENTRIES
                     
    #remove outliers outside +- IQR
    medx <- median(x)
    IQRx <- IQR(x)
    constant <- 10000
    x[x < (medx - constant)] <- NA
    x[x > (medx + constant)] <- NA
                     
    return(x)
})) %>% unlist()

entries_exits$ENTRIES2 <- entries_exits2

tmp <- entries_exits %>%
  arrange(STATION, desc(DATE), turnstile_ID) %>%
  group_by(STATION, DATE, turnstile_ID) %>%
  mutate(Total_entries = if_else(ENTRIES2 > lead(ENTRIES),
                                 ENTRIES2 - lead(ENTRIES),
                                 0)) %>%
  ungroup()

tmp %>%
  filter(unique_ID == "42ND ST - PORT AUTHORITY BUS TERM-27") %>% View()
  arrange(desc(turnstile_ID)) %>%
  View()

  x <- entries_exits[entries_exits$unique_ID == "42ND ST - PORT AUTHORITY BUS TERM-27", ]$ENTRIES
  
  #remove outliers outside +- IQR
  medx <- median(x)
  IQRx <- IQR(x)
  constant <- 10000
  x[x < (medx - constant)] <- NA
  x[x > (medx + constant)] <- NA
  x
  
  entries_exits[entries_exits$unique_ID == ID, ]$ENTRIES <<- x
  
tmp %>%
  group_by(STATION, DATE) %>%
  summarize(Total_entries = sum(Total_entries, na.rm = TRUE))  %>%
  arrange(desc(Total_entries)) %>%
  View()


#or remove entries that are above a certain threshold
invisible(
  lapply(unique(entries_exits$STATION),
         function(station) {
           group <- entries_exits[entries_exits$STATION == station,]$Total_entries
           entries_exits[entries_exits$STATION == station,]$Total_entries <<-
             case_when(
               group < 0 ~ 0,
               group < 100000 ~ group,
               group >= 100000 ~ 0)
         }
  )
)

#apply winsorizing at the station level
# invisible(
#   lapply(unique(entries_exits$STATION),
#          function(station) {
#            entries_exits[entries_exits$STATION == station,]$Total_entries <<-
#              entries_exits[entries_exits$STATION == station,]$Total_entries %>%
#              Winsorize(.,
#                        minval = 0,
#                        # maxval = 100000,
#                        probs = c(0, 0.9),
#                        na.rm = TRUE)
#          }
#   )
# )


range(entries_exits$Total_entries, na.rm = T)


entries_exits %>%
  group_by(STATION, DATE) %>%
  summarize(Total_entries = sum(Total_entries, na.rm = TRUE)) %>% 
  summarize(Total_entries = mean(Total_entries, na.rm = TRUE)) %>%
  arrange(desc(Total_entries)) %>%
  View()

entries_exits %>%
  filter(STATION == "PATH NEW WTC") %>%
  View()

entries_exits %>%
  group_by(STATION, DATE, TIME) %>%
  summarize(Total_entries = sum(Total_entries, na.rm = TRUE)) %>%
  ggplot(aes(x = TIME, y = Total_entries, group = DATE, color = DATE, alpha = 0.2)) +
  geom_line()



