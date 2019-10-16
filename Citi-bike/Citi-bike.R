library(tidyverse)
library(lubridate)
library(mapproj)
library(gridExtra)
source("Plots/ggplot-theme.R")
options(scipen = 999)

project.path <- getwd()



# data import and cleanup -------------------------------------------------------------

read_combine <- function(thisPath) {
  # function to import and combine the csvs within a file path
  # output is a dataframe of all the csv's rowbind'd together
  
  fileNames <- list.files(path = thisPath, pattern = "2019[0-1][1-9]-citibike-tripdata.csv")
  lapply(fileNames, function(fileName) {
    read_csv(
      paste(thisPath, fileName, sep = "/"),
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
  }) %>%
    bind_rows()
}

# read the data in, examine, and cleanup
bike.trips.df <- read_combine(paste0(project.path, "/Citi-bike/Data")) #read in 2019 data
rm(read_combine)
dim(bike.trips.df)
names(bike.trips.df) <- str_replace_all(names(bike.trips.df), " ", ".")
bike.trips.df$gender <- factor(bike.trips.df$gender, levels = c(0, 1, 2),
                                 labels = c("Unknown", "Male", "Female"))

# download nyc map shapes and clean them up
nyc.geojson <- httr::GET('https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON')
nyc.neighborhoods <- rgdal::readOGR(httr::content(nyc.geojson,'text'), 'OGRGeoJSON', verbose = FALSE)
summary(nyc.neighborhoods)
nyc.df <- broom::tidy(nyc.neighborhoods)

# id == 0 is the bronx
# id == 1 is staten island
# id == 2 is brooklyn
# id == 3 is queens
# id == 4 is manhattan

# check to see the shapefile works
ggplot() +
  geom_polygon(data = nyc.df, aes(x = long, y = lat, group = group), fill = "gray") +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.84))

# build df that has individual observations for start and end
# original data has start and end on the same row
tidy.bike.df.S <- bike.trips.df %>%
  select(starttime, stoptime, start.station.latitude, start.station.longitude, gender, birth.year) %>%
  mutate(Type = "Start") %>%
  rename(Lat = start.station.latitude,
         Long = start.station.longitude)

tidy.bike.df.E <- bike.trips.df %>%
  select(starttime, stoptime, end.station.latitude, end.station.longitude, gender, birth.year) %>%
  mutate(Type = "End") %>%
  rename(Lat = end.station.latitude,
         Long = end.station.longitude)

tidybike.df <- rbind(tidy.bike.df.S, tidy.bike.df.E)
rm(tidy.bike.df.S, tidy.bike.df.E)


# EDA ---------------------------------------------------------------------

# sample 1mm observations
samp_rows <- sample(nrow(tidybike.df), 1000000)

# plot trip ends and trip starts faceted commuting hours
tidybike.df[samp_rows,] %>%
  filter(wday(starttime) < 6) %>% #weekdays only
  mutate(Morning = hour(starttime) >= 6 & hour(starttime) <= 10,
         Evening = hour(starttime) >= 16 & hour(starttime) <= 20) %>%
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
  guides(color = guide_legend(override.aes = list(alpha = 1))) + #overrides the alpha in the legend only
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
#        path = project.path,
#        width = 8,
#        height = 7)


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

