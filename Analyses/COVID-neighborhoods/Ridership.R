source("Analyses/COVID-neighborhoods/helper_functions.R")
library(hms)
library(lubridate)
library(RSQLite)

# connect to database and read in data to memory --------------------------

# establish the connection to the database
conn <- dbConnect(RSQLite::SQLite(), "NYC.db")

# read in just September data into memory
turnstile.df <- tbl(conn, "turnstile.2020") %>%
  collect() %>%
  mutate(Date = as.Date(Date, origin = "1970-01-01"),
         Time = as_hms(Time))

# disconnect from the database
# dbDisconnect(conn)

# plot range for d3 axis
max.date <- as.Date("2020-09-21")
min.date <- as.Date("2020-01-01")
max.date.2019 <- as.Date("2019-09-21")
min.date.2019 <- as.Date("2019-01-01")


# all years ---------------------------------------------------------------

# read all daily ridership into memory
turnstile.df <- c()
tables <- c("turnstile.2019", "turnstile.2020") # dbListTables(conn) %>% str_subset("turnstile*")
for (i in tables) {
  turnstile.df <- tbl(conn, i) %>%
    filter(Desc == "REGULAR" | Desc == "RECOVR AUD") %>% 
    # calc entries by booth & SCP
    group_by(Booth, SCP) %>%
    mutate(Entries = Entries - lag(Entries)) %>% 
    ungroup() %>% 
    # trim outliers
    filter(Entries > 0,
           Entries <= 100000) %>% 
    group_by(Date, Station, Linename) %>%
    summarize(Daily.ridership = sum(Entries, na.rm = TRUE)) %>% 
    select(Date, Station, Linename, Daily.ridership) %>%
    collect() %>%
    mutate(Date = as.Date(Date, origin = "1970-01-01")) %>%
    na.omit() %>% 
    bind_rows(turnstile.df, .)
  gc()
}

# plot of ridership over time
daily_summary <- turnstile.df %>% 
  filter(Date >= as.Date("2020-01-01"),
         Date <= as.Date("2020-06-14")) %>% 
  group_by(Date) %>% 
  summarize(ridership = sum(Daily.ridership),
            .groups = 'drop')
daily_summary %>% 
  ggplot(aes(x = Date, y = ridership)) +
  geom_point(color = "#2b7551") +
  geom_line(color = "#2b7551") +
  scale_x_date(date_breaks = "1 month", date_labels = '%b') +
  scale_y_continuous(labels = scales::label_number(scale = 1 / 1e6, suffix = "M", accuracy = 1)) +
  labs(title = "Daily subway ridership drops significantly in March 2020",
       x = NULL,
       y = NULL)
# ggsave(filename = "Analyses/COVID-neighborhoods/Plots/ridership_timeseries.svg",
#        device = 'svg',
#        height = 4,
#        width = 8)

pre_mean <- daily_summary %>% filter(Date <= as.Date("2020-03-04")) %>% pull(ridership) %>% mean()
post_mean <- daily_summary %>% filter(Date >= as.Date("2020-4-06")) %>% pull(ridership) %>% mean()

daily_summary %>% 
  ggplot(aes(x = Date, y = ridership)) +
  geom_point(color = "grey50", alpha = 0.3) +
  geom_line(color = "grey50", alpha = 0.3) +
  geom_segment(aes(x = as.Date("2020-01-01"), xend = as.Date("2020-03-04"),
                 y = pre_mean, yend = pre_mean), color = '#2b7551', size = 1.5) +
  geom_segment(aes(x = as.Date("2020-04-06"), xend = as.Date("2020-06-14"),
                   y = post_mean, yend = post_mean), color = '#2b7551', size = 1.5) +
  geom_curve(aes(x =  as.Date("2020-02-25"), y = pre_mean * 1.05,
                 xend = as.Date("2020-04-15"), yend = post_mean * 1.4),
             curvature = -0.65, color = '#394E48', size = 1,
             arrow = arrow(type = 'closed', length = unit(0.4, "cm"))) +
  annotate("label", x =  as.Date("2020-05-06"), y = 4500000, 
           fill = '#394E48',
           label = paste0(scales::percent_format()(post_mean / pre_mean -1), 
                          ' drop in mean ridership'),
           color = 'white', size = 4, family = 'helvetica',
           label.size = 1.25, label.padding = unit(0.75, "lines")) +
  scale_x_date(date_breaks = "1 month", date_labels = '%b') +
  scale_y_continuous(labels = scales::label_number(scale = 1 / 1e6, suffix = "M", accuracy = 1)) +
  labs(title = "Resulting in a substantial drop in the mean ridership",
       x = NULL,
       y = NULL)
# ggsave(filename = "Analyses/COVID-neighborhoods/Plots/ridership_timeseries_bars.svg",
#        device = 'svg',
#        height = 4,
#        width = 8)

# match with lat long -----------------------------------------------------

# add lat long from database
turnstile.df <- turnstile.df %>%
  left_join(tbl(conn, "station.lat.long"), 
            by = c("Station", "Linename"), 
            copy = TRUE)


# maps --------------------------------------------------------------------

# map of nyc
ggplot() +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group, fill = id)) +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.82)) 

# veroni map of subway stations
# https://cran.r-project.org/web/packages/ggvoronoi/vignettes/ggvoronoi.html
library(ggvoronoi)
tbl(conn, "station.lat.long") %>% 
  collect() %>% 
  distinct(Lat, Long) %>%
  # mutate(distance = sqrt((Long-100)^2 + (Lat-100)^2)) %>% 
  ggplot(aes(x = Long, y = Lat)) +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "gray50") +
  geom_voronoi(color = 'white', alpha = 0.4) +
  scale_fill_continuous(type = 'viridis') +
  geom_point(color = 'grey20') +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.82))

# create df of change in ridership between pre and post covid
change_in_ridership <- turnstile.df %>% 
  group_by(Date, Lat, Long) %>% 
  summarize(Daily.ridership = sum(Daily.ridership),
            .groups = 'drop') %>%
  # filter(Date %in% c(as.Date("2020-04-08"), as.Date("2020-03-04"))) %>% 
  filter(Date >= as.Date("2020-01-01"),
         Date <= as.Date("2020-06-14")) %>% 
  mutate(pre_covid = Date <= as.Date("2020-03-09"),
         post_covid = Date >= as.Date("2020-04-06")) %>% 
  filter(pre_covid | post_covid) %>% 
  group_by(pre_covid, Lat, Long) %>% 
  summarize(total_ridership = mean(Daily.ridership),
            .groups = 'drop') %>% 
  group_by(Lat, Long) %>%
  arrange(pre_covid) %>% 
  mutate(ridership_change = total_ridership / lead(total_ridership) - 1) %>% 
  # mutate(ridership_change = Daily.ridership / lag(Daily.ridership) - 1) %>%
  ungroup() %>% 
  na.omit() %>% 
  filter(ridership_change > stats::quantile(ridership_change, probs = 0.025),
         ridership_change < stats::quantile(ridership_change, probs = 0.975)) %>% 
  select(-pre_covid)

# write out dataframe to use with Mapbox
# ridership_geojson <- change_in_ridership
# coordinates(ridership_geojson) <- c("Long", "Lat")
# rgdal::writeOGR(obj = ridership_geojson, 
#                 layer = "ridership_geojson",
#                 dsn = "Analyses/COVID-neighborhoods/Data/change_in_ridership.GeoJSON",
#                 driver = "GeoJSON")

# veroni heat map of change in subway ridership
change_in_ridership %>% 
  ggplot(aes(x = Long, y = Lat)) +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "gray40") +
  geom_voronoi(aes(fill = ridership_change), 
               color = 'grey85', alpha = 0.5, size = 0.3) +
               # outline = as.data.frame(nyc.df)) +
  scale_fill_continuous(type = 'viridis', 
                        name = "Change in station entries\n",
                        labels = scales::percent_format(accuracy = 1),
                        breaks = c(-0.95, -0.85, -0.75)) +
  geom_point(color = 'grey85', size = 0.4, alpha = 0.8) +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  coord_quickmap(xlim = c(-74.04, -73.79),
                 ylim = c(40.57, 40.88)) +
  labs(title = "Decline in subway ridership: pre- vs. post-Covid",
       subtitle = 'Color represents change in mean daily entries by station',
       caption = 'Jan 1-Mar 4 compared to Apr 6-Jun 14\nData: MTA turnstiles') +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = 'bottom',
        plot.caption = element_text(face = "italic",
                                    size = 6,
                                    color = 'grey50'))
# ggsave(filename = "Plots/change_in_ridership.svg",
#        device = 'svg',
#        height = 10,
#        width = 5.5)

# map of the cumulative monthly ridership for each subway station
turnstile.df %>%
  mutate(Post_covid = Date >= as.Date("2020-03-20")) %>% 
  group_by(Post_covid, Station, Linename, Lat, Long) %>%
  summarize(Ridership = mean(Daily.ridership, na.rm = TRUE)) %>%
  ggplot() +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "gray80") +
  geom_point(aes(x = Long, y = Lat, color = Ridership, size = Ridership), alpha = 0.4) +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.82)) +
  scale_color_continuous(guide = "none",
                         low = "#4aa87a", high = "#20593d") +
  scale_size_continuous(name = "Daily ridership",
                        labels = scales::comma) +
  facet_wrap(~Post_covid) +
  labs(title = "Monthly ridership for pre- and post-Covid",
       subtitle = "March 20, 2019") +
  light.theme +
  theme(axis.title = element_blank())


# old code ----------------------------------------------------------------

# daily ridership
turnstile.df %>%
  # remove outliers
  filter(Daily.ridership < 30000000) %>%
  ggplot(aes(x = Date, y = Daily.ridership)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  geom_line() +
  labs(title = 'Daily ridership drops significantly in March 2020',
       x = NULL,
       y = 'Daily ridership') +
  light.theme

# week of	2019-01-05 wasn't reported until 2019-01-12
# tibble(Date = seq(as.Date("2019-01-05"), as.Date("2019-01-12"), by = 1)) %>%
#   mutate(Daily.ridership = pull(turnstile.df[turnstile.df$Date == as.Date("2019-01-12"), 'Daily.ridership']) / 8) %>%
#   bind_rows(turnstile.df %>% filter(Date != as.Date("2019-01-12"))) %>%
#   arrange(Date)

# remove outlier week and non-Jan-March dates
turnstile.df <- turnstile.df %>% 
  filter(Date != as.Date("2019-01-12"),
         Date %in% seq(min.date.2019, max.date.2019 + 3, by = 1) | 
           Date %in% seq(min.date, max.date + 2, by = 1)) %>% 
  mutate(Month.Day = paste0(month(Date), '-', day(Date)))

# convert ridership to two column: one for 2019 and one for 2020
summ.ts <- turnstile.df %>% 
  filter(Date >= min.date) %>% 
  left_join(
    turnstile.df %>% 
      filter(Date < min.date) %>% 
      select(-Date),
    by = "Month.Day"
  ) %>% 
  select(date = Date, subway_2020 = Daily.ridership.x, subway_2019 = Daily.ridership.y)

# adjust dates to match weekdays
summ.ts <- summ.ts %>% 
  filter(date %in% seq(as.Date('2020-01-01'), as.Date('2020-02-28'), by = 1)) %>% 
  # lead one day for dates prior to the leap
  mutate(subway_2019  = lead(subway_2019 , n = 1)) %>% 
  select(-subway_2020) %>% 
  bind_rows(
    summ.ts %>% 
      filter(date %in% seq(as.Date('2020-02-29'), max.date + 2, by = 1)) %>% 
      # lead two days for dates after to the leap
      mutate(subway_2019  = lead(subway_2019 , n = 2)) %>% 
      select(-subway_2020) 
  ) %>% 
  left_join(summ.ts %>% select(-subway_2019)) %>% 
  select(date, subway_2020, subway_2019)

