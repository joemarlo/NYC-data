library(tidyverse)
library(httr)
library(hms)
library(lubridate)
library(DescTools)
library(stringdist)
library(parallel)
library(gganimate)
library(gifski)
source("Plots/ggplot-theme.R")

cpu.cores <- detectCores()


#function to read in the data, format, and then combine to one dataframe
read_subway_files<- function(year){
  filenames <- list.files(path = "Subway-turnstiles/Data/",
                          pattern = paste0("turnstile_", year, ".....txt"),
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

#read in the files
turnstile.df <- read_subway_files(19) %>% bind_rows()
rm(read_subway_files)

# data clean up-------------------------------------------------------------------------

# change names to proper case
toproper <- function(name) paste0(toupper(substr(name, 1, 1)), tolower(substring(name, 2)))
names(turnstile.df) <- sapply(names(turnstile.df), toproper)
names(turnstile.df)[1] <- "Booth"
names(turnstile.df)[3] <- "SCP"

#only include "regular" and "recover audit" measurements. Others include maintenance checks
turnstile.df <- turnstile.df %>% filter(Desc == "REGULAR" | Desc == "RECOVR AUD")

# calculate the difference in Entries within each grouping of Booth and  SCP (SCP is a group of turnstiles within a station)
# the goal here is the calculate the average difference in Entries for a given turnstile
# this assumes turnstiles are in the same order each time the data is updated by the MTA
# turnstile.df$diff <- ave(turnstile.df$Entries, turnstile.df$Booth, turnstile.df$SCP, FUN = function(x) c(0, diff(x)))
turnstile.df <- turnstile.df %>%
  group_by(Booth, SCP) %>%
  mutate(Entries = Entries - lag(Entries),
         Exits = Exits - lag(Exits))
  
# summary stats
summary(turnstile.df$Entries) %>% as.vector() %>% scales::comma()
summary(turnstile.df$Exits) %>% as.vector() %>% scales::comma()

# table of counts for given extreme breaks
breaks <- c(-Inf, 0, 1000, 10000, 100000, Inf)
hist(turnstile.df$Entries, breaks = breaks, plot = FALSE)$counts
hist(turnstile.df$Exits, breaks = breaks, plot = FALSE)$counts

# remove any negative counts
turnstile.df <- turnstile.df[turnstile.df$Entries > 0,]
turnstile.df <- turnstile.df[turnstile.df$Exits > 0,]

# remove anything over 100,000
# note: average daily ridership for the who system is 5.5mm
turnstile.df <- turnstile.df[turnstile.df$Entries <= 100000,]
turnstile.df <- turnstile.df[turnstile.df$Exits <= 100000,]

# check again for outliers
hist(turnstile.df$Entries, breaks = breaks, plot = FALSE)$counts
hist(turnstile.df$Exits, breaks = breaks, plot = FALSE)$counts
rm(breaks)

#net entries and exits
scales::percent((sum(turnstile.df$Entries, na.rm = TRUE) - sum(turnstile.df$Exits, na.rm = TRUE)) / sum(turnstile.df$Entries, na.rm = TRUE))

# EDA ---------------------------------------------------------------------

# top stations by day
turnstile.df %>%
  group_by(Station, Date) %>%
  summarize(Daily.ridership = sum(diff)) %>%
  group_by(Station) %>%
  summarize(Daily.ridership = mean(Daily.ridership)) %>%
  arrange(desc(Daily.ridership))

# station ridership by day of the week
turnstile.df %>%
  group_by(Date, Station) %>%
  summarize(Daily = sum(Entries)) %>%
  group_by(wday(Date), Station) %>%
  summarize(Daily = mean(Daily)) %>%
  rename(WeekDay = `wday(Date)`) %>%
  mutate(Color.group = case_when(Station == "34 ST-PENN STA" ~ "ColGroup1",
                                 Station == "GRD CNTRL-42 ST" ~ "ColGroup2",
                                 TRUE ~ "ColGroup3")) %>%
  ggplot(aes(x = WeekDay, y = Daily, group = Station, color = Color.group)) +
  geom_line(alpha = 0.4) +
  scale_colour_manual(values = c("forestgreen", "#2b7551", "gray80")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = 1:7,
                     labels = c("Sunday", "Monday", "Tuesday", "Wedneday", "Thursday", "Friday", "Saturday"),
                     name = NULL) +
  scale_y_continuous(labels = scales::comma,
                     name = "Daily station ridership") +
  labs(title = "Daily subway ridership by station",
       subtitle = "September 2019") +
  annotate("text", x = 6.7, y = 155000, label = 'bold("Penn")', parse =TRUE, color = "#2b7551") +
  annotate("text", x = 6.7, y = 140000, label = 'bold("GCT")', parse =TRUE, color = "forestgreen") +
  light.theme

# ggsave(filename = "Plots/Subway_ridership.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 7,
#        height = 5)


# add lat long ------------------------------------------------------------

# core problem is that the turnstile data doesn't have lat long info
# goal is to merge with a dataset known list of stations with lat long
# but dataset station names don't match so we need to fuzzy match

#import list of stations and tidy | need this for lat long
stations.latlong.df <- GET("https://data.cityofnewyork.us/api/views/kk4q-3rt2/rows.csv?accessType=DOWNLOAD")
stations.latlong.df <- content(stations.latlong.df)
write_csv(stations.latlong.df, "Subway-turnstiles/Data/stations.latlong.df.csv")

# change names to proper case
names(stations.latlong.df) <- sapply(names(stations.latlong.df), toproper)

#seperate out the lat/long
stations.latlong.df <- stations.latlong.df %>%
  mutate(The_geom = str_remove(The_geom, "POINT [(]") %>% str_remove(., "[)]")) %>%
  separate(The_geom, into = c("Long", "Lat"), sep = " ") %>%
  mutate(Lat = as.double(Lat),
         Long = as.double(Long)) %>%
  rename(Station = Name) %>%
  select(Station, Lat, Long, Line)

#check the station names; most station names don't match so need to fuzzy match
# unique(turnstile.df$Station)
# unique(stations.latlong.df$Station)

# need to match on station name and line; check line first then match name
# match subway line letters by splitting apart and checking individually
# need to clean up stations.latlong.df$Line first

# remove "express" so E doesn't match it
stations.latlong.df$Line <- str_remove_all(stations.latlong.df$Line, "Express")

# split out the letters from the lines column
stations.latlong.df$Line <- str_split(stations.latlong.df$Line, "-")
turnstile.df$Linename <- sapply(turnstile.df$Linename, function(line) str_split(line, ""))

# make station names lower case so they match better
stations.latlong.df$Station <- tolower(stations.latlong.df$Station)
turnstile.df$Station <- tolower(turnstile.df$Station)

# replace ave and avenue with av
stations.latlong.df$Station <- str_replace_all(stations.latlong.df$Station, " ave"," av")
stations.latlong.df$Station <- str_replace_all(stations.latlong.df$Station, "ave ","av ")
turnstile.df$Station <- str_replace_all(turnstile.df$Station, "avenue ","av ")
turnstile.df$Station <- str_replace_all(turnstile.df$Station, " ave"," av")

# replace street with st
turnstile.df$Station <- str_replace_all(turnstile.df$Station, " street"," st")

# replace road with rd
stations.latlong.df$Station <- str_replace_all(stations.latlong.df$Station, " road"," rd")
turnstile.df$Station <- str_replace_all(turnstile.df$Station, " road"," rd")

# replace place with pl
turnstile.df$Station <- str_replace_all(turnstile.df$Station, " place"," pl")

# remove "ths" and "rd" after street numbers
stations.latlong.df$Station[grep("[0-9]th", stations.latlong.df$Station)] <- str_remove_all(stations.latlong.df$Station[grep("[0-9]th", stations.latlong.df$Station)], "th")
stations.latlong.df$Station[grep("[0-9]rd", stations.latlong.df$Station)] <- str_remove_all(stations.latlong.df$Station[grep("[0-9]rd", stations.latlong.df$Station)], "rd")
turnstile.df$Station[grep("[0-9]th", turnstile.df$Station)] <- str_remove_all(turnstile.df$Station[grep("[0-9]th", turnstile.df$Station)], "th")

# remove "nd"s 
stations.latlong.df$Station[grep("[0-9]nd", stations.latlong.df$Station)] <- str_remove_all(stations.latlong.df$Station[grep("[0-9]nd", stations.latlong.df$Station)], "nd")


get_new_name <- function(old.name, subway.lines){
  
  # function returns a matching station name (plus line, lat, long) from the new lat/long dataset
  # it first creates a possible list of new stations based on which subway
  #   lines are matched
  # then it fuzzy matches the name of the old.name against the list
  #   of new station names
  
  # determine the probability a station matches by comparing the number of lines 
  # that match (the intersect) to the total amount (the union) of lines
  match.probs <- sapply(stations.latlong.df$Line, function(new.line) {
    n.intst <- length(intersect(subway.lines, new.line))
    n.union <- length(union(subway.lines, new.line))
    prob <- n.intst / n.union
    return(prob)
  })
  
  # find the station names that match the best: i.e. include matches that 
  #  are at least 50% as good as the top match
  match.stations <- stations.latlong.df[match.probs >= max(match.probs) * 0.5,]
  
  # now fuzzy match the station name within this list
  new.name <- stringsim(old.name, unlist(match.stations$Station), method = "osa") %>% which.max(.) %>% match.stations[.,]
  
  return(new.name)
}

# get only unique pairs of station and lines then get rid of NA line
station.line.pairs <- turnstile.df[,c("Station", "Linename")][!duplicated(turnstile.df[,c("Station", "Linename")]),]
station.line.pairs <- station.line.pairs[!is.na(station.line.pairs$Station),]

# apply the get_new_name function over the unique pairs data frame
station.line.pairs <- lapply(1:nrow(station.line.pairs), function(index) {
  get_new_name(station.line.pairs$Station[[index]], station.line.pairs$Linename[[index]])
}) %>% bind_rows() %>% bind_cols(station.line.pairs, .)

#rename the columns
station.line.pairs <- rename(station.line.pairs,
                             New.name = Station1,
                             New.line = Line)

# seems to still be some issues. the below 50 didn't match well (about 10% of total stations)
# 34 st-herald sq
# 14 st-union sq
# botanic garden
# union st
# 86 st [64]
# 181 st [132]
# canal st [156]
# world trade ct [158]
# 2 av [249]
# orchard beach [280]
# newark hw bmebe [281]
# harrson [282]
# journal square [283]
# grove st [284]
# exchange pl [285]
# pavonia [286]
# city / bus [287]
# chritopher [288]
# 9 st [289]
# 14 st [290]
# twenty third [291]
# thirty st [292]
# [293:299]
# 181 st [330]
# 231 st [336]
# spring [342]
# astor pl [344]
# 14 st union sq [345]
# 23rd [346]
# 28 st [347]
# 33 st [348]
# 51 st [350]
# 68st hunter [352]
# 96st [355]
# 103 [356]
# 110 [357]
# 116 [358]
# 33st rawson [427]
# 40 st lowery [428]
# 46 st bliss [429]
# 61 st woodside [431]
# flatbush av [467]
# st george [473]
# rit manhattan [474]
# rit roosevelt [475]
# newark [476]


#manually fix some of the big stations
station.line.pairs[6, 3:6] <- stations.latlong.df[145,] #herald sq
station.line.pairs[9, 3:6] <- stations.latlong.df[105,] #union sq - 14 st
station.line.pairs[24, 3:6] <- stations.latlong.df[281,] #botanic
station.line.pairs[44, 3:6] <- stations.latlong.df[36,] #union st
station.line.pairs[64, 3:6] <- stations.latlong.df[336,] # 86 st [64]
station.line.pairs[132, 3:6] <- stations.latlong.df[396,] # 181 st [132]
station.line.pairs[156, 3:6] <- stations.latlong.df[410,] # canal st [156]
station.line.pairs[158, 3:6] <- stations.latlong.df[409,] # world trade ct [158]
# station.line.pairs[249, 3:6] <- stations.latlong.df[,] # 2 av [249] no second ave in the dataset
station.line.pairs[288, 3:6] <- stations.latlong.df[195,] # christopher [288]
# station.line.pairs[289, 3:6] <- stations.latlong.df[195,] # 9 st [289] no ninth st in the dataset
station.line.pairs[290, 3:6] <- stations.latlong.df[439,] # 14 st [290]
station.line.pairs[291, 3:6] <- stations.latlong.df[96,] # twenty third [291]
# station.line.pairs[292, 3:6] <- stations.latlong.df[96,]# thirty st [292] no match in the dataset
station.line.pairs[330, 3:6] <- stations.latlong.df[178,] # 181 st [330]
station.line.pairs[336, 3:6] <- stations.latlong.df[267,] # 231 st [336]
station.line.pairs[342, 3:6] <- stations.latlong.df[467,] # spring [342]
station.line.pairs[344, 3:6] <- stations.latlong.df[1,] # astor pl [344]
station.line.pairs[345, 3:6] <- stations.latlong.df[105,] # 14 st union sq [345]
station.line.pairs[346, 3:6] <- stations.latlong.df[92,] # 23rd [346]
station.line.pairs[347, 3:6] <- stations.latlong.df[200,] # 28 st [347]
station.line.pairs[348, 3:6] <- stations.latlong.df[32,] # 33 st [348]
station.line.pairs[350, 3:6] <- stations.latlong.df[85,] # 51 st [350]
station.line.pairs[352, 3:6] <- stations.latlong.df[102,] # 68st hunter [352]
station.line.pairs[355, 3:6] <- stations.latlong.df[33,] # 96st [355]
station.line.pairs[356, 3:6] <- stations.latlong.df[458,] # 103 [356]
station.line.pairs[357, 3:6] <- stations.latlong.df[450,] # 110 [357]
station.line.pairs[358, 3:6] <- stations.latlong.df[462,] # 116 [358]

# process to manually look up matches
# indices <- grep("116", stations.latlong.df$Station)
# View(stations.latlong.df %>% rowid_to_column() %>% .[indices,], title = "matches")
# View(station.line.pairs[358,], title = "actual")

# histogram of string sim scores of the station name matches
station.line.pairs %>%
  rowwise() %>%
  mutate(Score = stringsim(Station, New.name)) %>%
  ggplot(aes(x = Score)) +
  geom_histogram(color = "white", binwidth = 0.05) +
  labs(title = "Histogram of the string match scores comparing the final station names",
       subtitle = "Higher is better; 1 indicates perfect match")

# unlist the new name and line
station.line.pairs$Linename <- sapply(station.line.pairs$Linename, function(x) unlist(x) %>% paste(., collapse = "-"))
turnstile.df$Linename <- sapply(turnstile.df$Linename, function(x) unlist(x) %>% paste(., collapse = "-"))
station.line.pairs$New.name <- sapply(station.line.pairs$New.name, unlist)

# merge with the new data frame to get lat long
turnstile.clean.df <- left_join(x = turnstile.df, y = station.line.pairs, by = c("Station", "Linename"))
turnstile.clean.df <- turnstile.clean.df %>% rename(Old.station.name = Station, Station = New.name)
rm(station.line.pairs)

# visuals -----------------------------------------------------------------

# map of the cumulative monthly ridership for each subway station
turnstile.clean.df %>%
  group_by(Station, Linename, Lat, Long) %>%
  summarize(Monthly.ridership = sum(Entries)) %>%
  ggplot() +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "gray80") +
  geom_point(aes(x = Long, y = Lat, color = Monthly.ridership, size = Monthly.ridership), alpha = 0.4) +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.82)) +
  scale_color_continuous(guide = "none",
                         low = "#4aa87a", high = "#20593d") +
  scale_size_continuous(name = "Monthly ridership",
                        labels = scales::comma) +
  labs(title = "Monthly ridership for each subway station",
       subtitle = "September 2019") +
  light.theme +
  theme(axis.title = element_blank())

# ggsave(filename = "Plots/Subway_map.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 8,
#        height = 6)

# map segmented by commuting hours
turnstile.clean.df %>%
  ungroup() %>%
  select(Date, Time, Station, Lat, Long, Entries, Exits) %>%
  filter(wday(Date) %in% 2:6) %>%
  mutate(Morning = hour(Time) >= 6 & hour(Time) <= 10,
         Evening = hour(Time) >= 16 & hour(Time) <= 20) %>%
  filter(Morning | Evening) %>%
  gather(key = "Type", value = "Count", -c("Date", "Time", "Station", "Lat", "Long", "Morning", "Evening")) %>%
  mutate(Time.of.Day = if_else(Morning, "6am-10am", if_else(Evening, "4pm-8pm", "NULL")), #collpase Morning/Evening into one variable
         Time.of.Day = factor(Time.of.Day, levels = c("6am-10am", "4pm-8pm")), #reorder so facets are ordered correctly
         Type = factor(Type, levels = c("Entries", "Exits"))) %>% #reorder so legend is ordered correctly
  group_by(Station, Lat, Long, Type, Time.of.Day) %>%
  summarize(Monthly.ridership = sum(Count)) %>%
  ggplot() +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "gray80") +
  geom_point(aes(x = Long, y = Lat, color = Type, size = Monthly.ridership), alpha = 0.4) +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.82)) +
  scale_color_manual(values = c("darkblue", "indianred2"),
                     name = NULL,
                     guide = "none") +
  scale_size_continuous(name = "Monthly ridership",
                        labels = scales::comma) +
  labs(title = "Monthly ridership for each subway station",
       subtitle = "September 2019") +
  theme(legend.position = "bottom") +
  light.theme +
  theme(axis.title = element_blank(),
        panel.spacing.x = unit(2, "lines")) +
  facet_grid(Type ~ Time.of.Day)

# ggsave(filename = "Plots/Subway_time_map.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 8,
#        height = 11)

#create animated map of ridership hourly
subway.plot <- turnstile.clean.df %>%
  filter(Time %in% as_hms(paste0(2:20, ":00:00"))) %>% #only retain on the hour times (causes issue with animation)
  mutate(Time = hour(Time),
         id = paste0(Station, "-", Lat, "-", Long)) %>%
  group_by(Station, Lat, Long, id, Time) %>%
  summarize(Hourly.ridership = sum(Entries) / (30*4)) %>% # change from monthly to hourly
  ggplot() +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "gray80") +
  geom_point(aes(x = Long, y = Lat,
                 group = id,
                 color = Hourly.ridership,
                 size = Hourly.ridership),
             alpha = 0.4) +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.82)) +
  scale_color_continuous(guide = "none",
                         low = "#4aa87a", high = "#20593d") +
  scale_size_continuous(name = "Hourly ridership",
                        labels = scales::comma) +
  transition_reveal(along = Time) +
  labs(title = "Hourly ridership for each subway station",
       subtitle = "Time: {frame_along}:00") +
  light.theme +
  theme(axis.title = element_blank())

#build the animation
subway.gif <- animate(subway.plot,
                      fps = 24,
                      duration = 12,
                      end_pause = 24 * 2, #pause for 2 seconds at the end
                      height = 576,
                      width = 432)

# anim_save(animation = subway.gif,
#           filename = "Plots/subway_hourly.gif")



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


new.names <- lapply(1:nrow(turnstile.df), function(line) {
  
  subway.lines <- turnstile.df$Linename[[line]]
  old.name <- turnstile.df$Station[[line]]
  
  # count of how many stations are matched in the new data
  match.counts <- sapply(stations.latlong.df$Line, function(x) sum(subway.lines %in% x))
  
  # find the station names that match within one stations
  match.stations <- stations.latlong.df$Station[match.counts >= max(match.counts) - 2]
  
  # now fuzzy match the station name within this list
  new.name <- stringsim(old.name, match.stations, method = "osa") %>% which.max(.) %>% match.stations[.]
  
  return(new.name)
})


#match the original names to the new names using stringsim method
orig_names <- unique(turnstile.df$Station)
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


# recreate thte first plot to see if it still makes sense:  station ridership by day of the week
turnstile.clean.df.2 %>%
  group_by(Date, Station, Linename, Lat, Long) %>%
  summarize(Daily = sum(diff)) %>%
  group_by(wday(Date), Station, Linename, Lat, Long) %>%
  summarize(Daily = mean(Daily)) %>%
  rename(WeekDay = `wday(Date)`) %>%
  mutate(Color.group = case_when(Station == "grand central - 42nd st" ~ "ColGroup1",
                                 Station == "herald sq - 34th st" ~ "#ColGroup2",
                                 TRUE ~ "ColGroup3")) %>%
  ggplot(aes(x = WeekDay, y = Daily, group = Station, color = Color.group)) +
  geom_line(alpha = 0.4) +
  scale_colour_manual(values = c("forestgreen", "#2b7551", "gray80")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = 1:7,
                     labels = c("Monday", "Tuesday", "Wedneday", "Thursday", "Friday", "Saturday", "Sunday"),
                     name = NULL) +
  scale_y_continuous(labels = scales::comma,
                     name = "Daily station ridership") +
  labs(title = "Daily subway ridership by station",
       subtitle = "September 2019") +
  annotate("text", x = 6.7, y = 155000, label = 'bold("GCT")', parse =TRUE, color = "#2b7551") +
  annotate("text", x = 6.7, y = 140000, label = 'bold("34th St")', parse =TRUE, color = "forestgreen") +
  light.theme

# using the preprocessed lat long data ------------------------------------

#import list of stations and tidy | need this for lat long
github.latlong.df <- read_csv("https://raw.githubusercontent.com/chriswhong/nycturnstiles/master/geocoded.csv", 
                              col_names = FALSE)
#rename the columns
names(github.latlong.df) <- c("Unit", "Booth", "Station", "Lines", "Division", "Lat", "Long")
#join it with the original turnstile data
turnstile.clean.df.2 <- left_join(turnstile.df, github.latlong.df[, c("Unit", "Booth", "Lat", "Long")], by = c("Booth", "Unit"))

