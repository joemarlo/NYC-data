library(tidyverse)
library(httr)
library(hms)
library(lubridate)
library(DescTools)
library(stringdist)
library(gganimate)
library(gifski)
library(RSQLite)
source("Plots/ggplot-theme.R")


# connect to database and read in data to memory --------------------------

# establish the connection to the database
conn <- dbConnect(RSQLite::SQLite(), "NYC.db")

# read in just September data into memory
turnstile.df <- tbl(conn, "turnstile.2019") %>%
  filter(Source.file == "turnstile_190921.txt" |
         Source.file == "turnstile_190928.txt" |
         Source.file == "turnstile_190914.txt" |
         Source.file == "turnstile_190907.txt") %>%
  collect() %>%
  mutate(Date = as.Date(Date, origin = "1970-01-01"),
         Time = as_hms(Time))
  
# can also query on disk like this
# tmp <- tbl(conn, "turnstile.2019.09")
# tmp %>%
#   select(Station, Time, Entries, Exits) %>%
#   group_by(Station) %>%
#   summarize(Entries = sum(Entries),
#             Exits = sum(Exits))

# disconnect from the database
# dbDisconnect(conn)

# helper functions --------------------------------------------------------

# change names to proper case
toproper <- function(name) paste0(toupper(substr(name, 1, 1)), tolower(substring(name, 2)))


# data clean up-------------------------------------------------------------------------

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

# net entries and exits
scales::percent((sum(turnstile.df$Entries, na.rm = TRUE) -
                 sum(turnstile.df$Exits, na.rm = TRUE)) /
                 sum(turnstile.df$Entries, na.rm = TRUE))

# remove NA rows
turnstile.df <- na.omit(turnstile.df)

# add lat long from database
turnstile.df <- turnstile.df %>%
  left_join(tbl(conn, "station.lat.long"), by = c("Station", "Linename"), copy = TRUE)

# EDA ---------------------------------------------------------------------

# top stations by day
turnstile.df %>%
  group_by(Station, Date) %>%
  summarize(Daily.ridership = sum(Entries)) %>%
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


# visuals -----------------------------------------------------------------

# map of the cumulative monthly ridership for each subway station
turnstile.df %>%
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
turnstile.df %>%
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
  labs(title = "Monthly ridership for each subway station during commuting hours",
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
subway.plot <- turnstile.df %>%
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


