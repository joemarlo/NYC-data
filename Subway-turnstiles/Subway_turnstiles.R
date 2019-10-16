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
rm(warpbreaks)

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
  mutate(Color.group = case_when(Station == "34 ST-PENN STA" ~ "ColGroup1",
                                 Station == "GRD CNTRL-42 ST" ~ "#ColGroup2",
                                 TRUE ~ "ColGroup3")) %>%
  ggplot(aes(x = WeekDay, y = Daily, group = Station, color = Color.group)) +
  geom_line() +
  scale_colour_manual(values = c("forestgreen", "#2b7551", "gray80")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = 1:7,
                     labels = c("Monday", "Tuesday", "Wedneday", "Thursday", "Friday", "Saturday", "Sunday"),
                     name = NULL) +
  scale_y_continuous(labels = scales::comma,
                     name = "Daily station ridership") +
  labs(title = "Daily subway ridership by station",
       subtitle = "September 2019") +
  annotate("text", x = 6.5, y = 180000, label = 'bold("Penn")', parse =TRUE, color = "forestgreen") +
  annotate("text", x = 6.5, y = 160000, label = 'bold("GCT")', parse =TRUE, color = "#2b7551") +
  light.theme

# ggsave(filename = "Plots/Subway_ridership.svg",
#        plot = last_plot(),
#        device = "svg",
#        path = project.path,
#        width = 7,
#        height = 5)


# add lat long ------------------------------------------------------------

# core problem is that the turnstile data doesn't have lat long info
# goal is to merge with a dataset known list of stations with lat long
# but dataset station names don't match so we need to fuzzy match

#import list of stations and tidy | need this for lat long
stations.df <- GET("https://data.cityofnewyork.us/api/views/kk4q-3rt2/rows.csv?accessType=DOWNLOAD")
stations.df <- content(stations.df)

# change names to proper case
names(stations.df) <- sapply(names(stations.df), function(name) paste0(toupper(substr(name, 1, 1)), tolower(substring(name, 2))))

#seperate out the lat/long
stations.df <- stations.df %>%
  mutate(The_geom = str_remove(The_geom, "POINT [(]") %>% str_remove(., "[)]")) %>%
  separate(The_geom, into = c("Lat", "Long"), sep = " ") %>%
  mutate(Lat = as.double(Lat),
         Long = as.double(Long)) %>%
  rename(Station = Name) %>%
  select(Station, Lat, Long, Line)

#check the station names; most station names don't match so need to fuzzy match
unique(turnt$Station)
unique(stations.df$Station)

# need to match on station name and line; check line first then match name?
# match subway line letters by splitting apart and checking individually
# need to clean up stations.df$Line first; remove "express" so E doesn't match it
stations.df$Line <- lapply(stations.df$Line, function(line) str_remove_all(line, "Express"))
stations.df$Line <- sapply(stations.df$Line, function(line) str_split(line, "-"))

# similarly clean up the equavalent column in turnt
turnt$Linename <- sapply(turnt$Linename, function(line) str_split(line, ""))


get_new_name <- function(old.name, subway.lines){
  
  # function returns a matching station name from the new lat/long dataset
  # it first creates a possible list of new station names based on which subway
  #   lines are served
  # then it fuzzy matches the name of the old.name against the list
  #   of new station news
  
  # count of how many stations are matched in the new data
  match.counts <- sapply(stations.df$Line, function(x) sum(subway.lines %in% x))
  
  # find the station names that match within two stations or at least one station
  match.stations <- stations.df$Station[match.counts >= max(1, max(match.counts) - 2)]
  
  # now fuzzy match the station name within this list
  new.name <- stringsim(old.name, match.stations, method = "osa") %>% which.max(.) %>% match.stations[.]
  
  return(new.name)
}


# get only unique pairs of station and lines
station_line_pairs <- turnt[,c("Station", "Linename")][!duplicated(turnt[,c("Station", "Linename")]),]

# apply the get_new_name function over the unique pairs data frame
station_line_pairs$New.name <- lapply(1:nrow(station_line_pairs), function(index) {
  get_new_name(station_line_pairs$Station[[index]], station_line_pairs$Linename[[index]])
})

# seems to still be some issues. e.g. below stations
# 42 ST-PORT AUTH
# 34 ST-HERALD SQ

# unlist the new name and line
station_line_pairs$Linename <- sapply(station_line_pairs$Linename, function(x) unlist(x) %>% paste(., collapse = "-"))
turnt$Linename <- sapply(turnt$Linename, function(x) unlist(x) %>% paste(., collapse = "-"))
station_line_pairs$New.name <- sapply(station_line_pairs$New.name, unlist)

# merge with the new data frame to get lat long
turnt_clean <- left_join(x = turnt, y = station_line_pairs, by = c("Station", "Linename"))
turnt_clean <- turnt_clean %>% rename(Old.station.name = Station, Station = New.name)
turnt_clean <- left_join(x = turnt_clean, y = stations.df[, c("Station", "Lat", "Long")], by = "Station")

# download nyc map shapes and clean them up
nyc.geojson <- httr::GET('https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON')
nyc.neighborhoods <- rgdal::readOGR(httr::content(nyc.geojson,'text'), 'OGRGeoJSON', verbose = FALSE)
summary(nyc.neighborhoods)
nyc.df <- broom::tidy(nyc.neighborhoods)

# map of the cumulative monthly ridership for each subway station
turnt_clean %>%
  group_by(Station) %>%
  summarize(Monthly.ridership = sum(diff),
            Lat = mean(Lat),
            Long = mean(Long)) %>%
  ggplot() +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "gray80") +
  geom_point(aes(x = Lat, y = Long, color = Monthly.ridership, size = Monthly.ridership)) +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.82)) +
  scale_color_continuous(guide = "none") +
  scale_size_continuous(name = "Monthly ridership",
                        labels = scales::comma) +
  labs(title = "Monthly ridership for each subway station",
       subtitle = "September 2019",
       caption = "Messy data; not reliable") +
  light.theme +
  theme(axis.title = element_blank())

ggsave(filename = "Plots/Subway_map.svg",
       plot = last_plot(),
       device = "svg",
       path = project.path,
       width = 7,
       height = 5)

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


new.names <- lapply(1:nrow(turnt), function(line) {
  
  subway.lines <- turnt$Linename[[line]]
  old.name <- turnt$Station[[line]]
  
  # count of how many stations are matched in the new data
  match.counts <- sapply(stations.df$Line, function(x) sum(subway.lines %in% x))
  
  # find the station names that match within one stations
  match.stations <- stations.df$Station[match.counts >= max(match.counts) - 2]
  
  # now fuzzy match the station name within this list
  new.name <- stringsim(old.name, match.stations, method = "osa") %>% which.max(.) %>% match.stations[.]
  
  return(new.name)
})


#match the original names to the new names using stringsim method
orig_names <- unique(turnt$Station)
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
