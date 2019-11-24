library(tidyverse)
library(lubridate)
library(mapproj)
library(gridExtra)
library(RSQLite)
source("Plots/ggplot-theme.R")
options(scipen = 999)

# connect to database and read in data to memory --------------------------

# establish the connection to the database
conn <- dbConnect(RSQLite::SQLite(), "NYC.db")

# read in all of 2019 data
bike.trips.df <- tbl(conn, "citibike.2019") %>%
  collect() %>%
  mutate(Starttime = as_datetime(Starttime),
         Stoptime = as_datetime(Stoptime),
         Gender = factor(Gender, levels = c("Unknown", "Male", "Female")))

# can also query on disk like this
# tmp <- tbl(conn, "turnstile.2019.09")
# tmp %>%
#   select(Station, Time, Entries, Exits) %>%
#   group_by(Station) %>%
#   summarize(Entries = sum(Entries),
#             Exits = sum(Exits))

# disconnect from the database
# dbDisconnect(conn)


# tidy up the data ---------------------------------------------------------------------

# build df that has individual observations for start and end
# original data has start and end on the same row
tidy.bike.df.S <- bike.trips.df %>%
  select(Starttime, Stoptime, Start.station.latitude, Start.station.longitude, Gender, Birth.year) %>%
  mutate(Type = "Start") %>%
  rename(Lat = Start.station.latitude,
         Long = Start.station.longitude)

tidy.bike.df.E <- bike.trips.df %>%
  select(Starttime, Stoptime, End.station.latitude, End.station.longitude, Gender, Birth.year)  %>%
  mutate(Type = "End") %>%
  rename(Lat = End.station.latitude,
         Long = End.station.longitude)

tidybike.df <- rbind(tidy.bike.df.S, tidy.bike.df.E)
rm(tidy.bike.df.S, tidy.bike.df.E)


# EDA ---------------------------------------------------------------------

# sample 1mm observations
samp_rows <- sample(nrow(tidybike.df), 1000000)

# plot trip ends and trip starts faceted commuting hours
tidybike.df[samp_rows,] %>%
  filter(wday(Starttime) < 6) %>% #weekdays only
  mutate(Morning = hour(Starttime) >= 6 & hour(Starttime) <= 10,
         Evening = hour(Starttime) >= 16 & hour(Starttime) <= 20) %>%
  filter(Morning | Evening) %>%
  rowwise() %>%
  mutate(Time.of.Day = if_else(Morning, "6am-10am", if_else(Evening, "4pm-8pm", "NULL")), #collpase Morning/Evening into one variable
         Time.of.Day = factor(Time.of.Day, levels = c("6am-10am", "4pm-8pm")), #reorder so facets are ordered correctly
         Type = factor(Type, levels = c("Start", "End"))) %>% #reorder so legend is ordered correctly
  ungroup() %>%
  ggplot() +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "gray80") +
  geom_point(aes(x = Long, y = Lat, color = Type), size = 2.5, alpha = 0.05) +
  scale_color_manual(values = c("darkblue", "indianred2"),
                     name = NULL) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) + #overrides the alpha in the legend only; makes the legend legible
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.82)) +
  labs(title = "Start and end locations of Citi Bike trips during commuting hours",
       subtitle = paste0("A sample of ",
                         scales::comma(length(samp_rows)),
                         " weekday trips in 2019")) +
  theme(legend.position = "bottom") +
  light.theme +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.spacing.x = unit(2, "lines")) +
  facet_grid(~ Time.of.Day)

# ggsave(filename = "Plots/Commuting_light.png",
#        plot = last_plot(),
#        device = "png",
#        width = 8,
#        height = 7)

# count number of trips per day, one table at a time
#  then combine into one dataframe
#  needs to be a loop b/c it dumps the memory after
#  summarizing each table
tables <- dbListTables(conn) %>% grep("citibike*", ., value = TRUE)
date.counts <- data.frame()
for (table in tables){
 tmp <- tbl(conn, table) %>%
  collect() %>%
  group_by(Date = date(as_datetime(Starttime))) %>%
  summarize(n.rides = n())
 date.counts <- bind_rows(date.counts, tmp)
}

# plot of daily count of trips by date
date.counts %>%
  ggplot(aes(x = Date, y = n.rides)) +
  geom_point(alpha = 0.5,
             shape = 19,
             color = "#2b7551") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Daily count of Citi Bike rides",
       x = "",
       y = "Daily rides") +
  light.theme +
  theme(legend.position = "none")

# ggsave(filename = "Plots/Daily_bike_rides.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 9,
#        height = 5)


# plot of daily count of trips by date and temperature
# add weather from database
tbl(conn, "Central.Park.weather") %>%
  collect() %>%
  mutate(Date = as_date(Date),
         Max.temp.avg = zoo::rollapply(Max.temp, width = 30, mean, align = 'center', fill = NA),
         Min.temp.avg = zoo::rollapply(Min.temp, width = 30, mean, align = 'center', fill = NA)) %>%
  right_join(date.counts, by = "Date") %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = n.rides),
             alpha = 0.4,
             shape = 19,
             color = "#2b7551") +
  geom_ribbon(aes(ymax = Max.temp.avg*700,
                  ymin = Min.temp.avg*700),
              fill = "grey30",
              alpha = 0.5) +
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~./700, name = "Temperature range")) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Daily count of Citi Bike rides",
       x = "",
       y = "Daily rides") +
  light.theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = NA))


tables <- dbListTables(conn) %>% grep("citibike*", ., value = TRUE)
minute.counts <- data.frame()
for (table in tables) {
  
  # collect one year's worth of data
  this.year <- tbl(conn, table) %>% collect()
  
  # count number of days in the year
  n.days <- n_distinct(date(as_datetime(this.year$Starttime)))
  
  # count the number of mean rides per minute
  tmp <- this.year %>%
    group_by(Hour = hour(as_datetime(Starttime)),
             Minute = minute(as_datetime(Starttime))) %>%
    tally() %>%
    mutate(n = n / n.days) %>%
    ungroup() %>%
    mutate(id = 1:(60 * 24), # add unique identifier for the minute
           year = table)
  
  # build one data frame of all the years
  minute.counts <- bind_rows(minute.counts, tmp)
}

# set colors for years
# year.colors <- seq_gradient_pal(low = "#5670a8",
#                                 high = "#EE6363",
#                                 space = "Lab")(seq(0, 1, length = 7))
year.colors <- c(rep("#bdc1c7", 6), "#2b7551")

# plot of average count of trips by minute
minute.counts %>%
  ggplot(aes(x = id, y = n, group = year, color = year)) +
  geom_line() +
  scale_color_manual(values = year.colors) +
  scale_x_continuous(labels = 0:24, breaks = seq(0, 60*24, by = 60)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Count of Citi Bike rides starting every minute",
       x = "Hour of day",
       y = "Average number of rides starting each minute") +
  light.theme +
  annotate("text", x = 20*60, y = 90,
           label = 'bold("2019")', parse =TRUE, color = "#2b7551") +
  theme(legend.position = "none")

# ggsave(filename = "Plots/Minute_bike_rides.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 9,
#        height = 5)

# scratch code ------------------------------------------------------------

#bike trips from our station
bike.trips.df[grep("110 St & Madison", bike.trips.df$start.station.name),] %>%
  ggplot(aes(x = tripduration / 60 / 60)) +
  geom_histogram(color = "white") +
  scale_x_log10()

#trip duration plotted by age group and gender
bike.trips.df %>%
  mutate(
    age.group = case_when(
      (2019 - birth.year) > 70 ~ "70+",
      (2019 - birth.year) > 60 ~ "60-70",
      (2019 - birth.year) > 50 ~ "50-60",
      (2019 - birth.year) > 40 ~ "40-50",
      (2019 - birth.year) > 30 ~ "30-40",
      (2019 - birth.year) > 20 ~ "20-30",
      (2019 - birth.year) > 10 ~ "10-20",
      (2019 - birth.year) > 0 ~ "0-10"
    )
  ) %>%
  ggplot(aes(x = tripduration / 60 / 60)) +
  geom_histogram(color = "white") +
  scale_x_log10() +
  facet_grid(gender ~ age.group, scales = "free")

bike.trips.df %>%
  mutate(Year = year(starttime)) %>%
  group_by(Year, gender) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = gender, y = n)) +
  geom_col() +
  facet_grid(~Year)

#next steps; look at "drunk" rides on the weekend after 12am
#hist of rides by starttime
bike.trips.df %>%
  mutate(Time = hour(starttime),
         Year = year(starttime)) %>%
  ggplot(aes(x = Time)) +
  geom_histogram(color = "white", binwidth = 1) +
  facet_wrap(~Year)

#check that plot work
bike.trips.df[sample(nrow(bike.trips.df), 100000),] %>%
  select(start.station.latitude, start.station.longitude) %>%
  ggplot() +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "grey") +
  geom_point(aes(x = start.station.longitude, y = start.station.latitude),
             alpha = 0.5) +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.84))

#plot trip ends and trip starts

tidybike.df[samp_rows,] %>%
  ggplot() +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "grey") +
  geom_point(aes(x = Long, y = Lat, color = Type), alpha = 0.1) +
  scale_color_manual(values = c("green", "red")) +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.84)) +
  theme_void() +
  theme(legend.position = "none") +
  facet_grid(~gender)

