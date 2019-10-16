library(tidyverse)
library(httr)
library(hms)
library(lubridate)
library(DescTools)
library(stringdist)
library(parallel)

cpu.cores <- detectCores()
# there are some major data cleaning issues

turnt <- read_csv("Subway-turnstiles/Data/turnstile_190928.txt",
                  col_types = cols(
                    DATE = col_date(format = "%m/%d/%Y"),
                    ENTRIES = col_double(),
                    EXITS = col_double(),
                    TIME = col_time(format = "%H:%M:%S")
                    )
                  )

# data clean up-------------------------------------------------------------------------

# change names to proper case
names(turnt) <- sapply(names(turnt), function(name) paste0(toupper(substr(name, 1, 1)), tolower(substring(name, 2))))
names(turnt)[1] <- "Booth"
names(turnt)[3] <- "SCP"

#only include "regular" and "recover audit" measurements. Others include maintenance checks
turnt <- turnt %>% filter(Desc == "REGULAR" | Desc == "RECOVR AUD")

# calculate the difference in Entries within each grouping of Booth and  SCP (SCP is a group of turnstiles within a station)
# the goal here is the calculate the average difference in Entries for a given turnstile
# this assumes turnstiles are in the same order each time the data is updated by the MTA
turnt$diff <- ave(turnt$Entries, turnt$Booth, turnt$SCP, FUN = function(x) c(0, diff(x)))

# here's a simpler example
attach(warpbreaks)
warpbreaks$diff <- ave(breaks, tension, FUN = function(x) c(0, diff(x)))
warpbreaks
detach(warpbreaks)

# summary stats
summary(turnt$diff) %>% as.vector() %>% scales::comma()

# table of counts for given extreme breaks
breaks <- c(-Inf, 0, 1000, 10000, 100000, Inf)
hist(turnt$diff, breaks = breaks, plot = FALSE)$counts

# remove any negative counts
turnt <- turnt[turnt$diff > 0,]

# remove anything over 100,000
# note: average daily ridership for the who system is 5.5mm
turnt <- turnt[turnt$diff <= 100000,]

# check again for outliers
hist(turnt$diff, breaks = breaks, plot = FALSE)$counts
rm(breaks)


# EDA ---------------------------------------------------------------------

# top stations by day
turnt %>%
  group_by(Station, Date) %>%
  summarize(Daily = sum(diff)) %>%
  arrange(desc(Daily))

# station ridership by day of the week
turnt %>%
  group_by(wday(Date), Station) %>%
  summarize(Daily = sum(diff)) %>%
  rename(WeekDay = `wday(Date)`) %>%
  ggplot(aes(x = WeekDay, y = Daily, group = Station, color = Station)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = 1:7,
                     labels = c("Monday", "Tuesday", "Wedneday", "Thursday", "Friday", "Saturday", "Sunday")) +
  scale_y_continuous(labels = scales::comma,
                     name = "Daily station ridership") +
  labs(title = "Daily subway ridership by station",
       subtitle = "September 2019") +
  light.theme

# add lat long ------------------------------------------------------------

# core problem is that the turnstile data doesn't have lat long
# goal is to merge with a dataset known list of stations with lat long
# but dataset station names don't match so we need to fuzzy match

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



# scratch code ------------------------------------------------------------

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


# potential winsoring methodologies below
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


# or remove entries that are above a certain threshold
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

# apply winsorizing at the station level
invisible(
  lapply(unique(entries_exits$STATION),
         function(station) {
           entries_exits[entries_exits$STATION == station,]$Total_entries <<-
             entries_exits[entries_exits$STATION == station,]$Total_entries %>%
             Winsorize(.,
                       minval = 0,
                       # maxval = 100000,
                       probs = c(0, 0.9),
                       na.rm = TRUE)
         }
  )
)


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
