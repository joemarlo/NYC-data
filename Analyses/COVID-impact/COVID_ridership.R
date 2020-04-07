library(tidyverse)
library(hms)
library(lubridate)
library(RSQLite)
source('Plots/ggplot-theme.R')

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


# Plots -------------------------------------------------------------------

# daily ridership
summ.ts <- turnstile.df %>%
  filter(Date >= as.Date('2020-01-01')) %>% 
  group_by(Date) %>%
  summarize(Daily.ridership = sum(Entries)) %>% 
  rename(date = Date,
         value = Daily.ridership)

summ.ts %>%
  ggplot(aes(x = date, y = value)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  geom_line(color = '#2b7551') +
  geom_point(color = '#2b7551') +
  labs(title = 'Daily subway ridership drops significantly in March 2020',
       x = NULL,
       y = 'Daily ridership',
       caption = 'Data: MTA turnstiles\nmarlo.works') +
  light.theme +
  theme(plot.caption = element_text(face = "italic",
                                    size = 6,
                                    color = 'grey50'))

# ggsave('Plots/COVID_ridership.svg',
#        device = 'svg',
#        width = 9,
#        height = 5)


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
    group_by(Date) %>%
    summarize(Daily.ridership = sum(Entries)) %>% 
    select(Date, Daily.ridership) %>%
    collect() %>%
    mutate(Date = as.Date(Date, origin = "1970-01-01")) %>%
    na.omit() %>% 
    bind_rows(turnstile.df, .)
  gc()
}

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

# ggsave('Plots/COVID_ridership_full.svg',
#        device = 'svg',
#        width = 9,
#        height = 5)


# citibike ----------------------------------------------------------------

# read in all of 2019:2020 data
bike.trips.df <- bind_rows(
  tbl(conn, "citibike.2019") %>%
    collect() %>%
    mutate(
      Starttime = as_datetime(Starttime),
      Stoptime = as_datetime(Stoptime),
      Gender = factor(Gender, levels = c("Unknown", "Male", "Female"))
    ),
  tbl(conn, "citibike.2020") %>%
    collect() %>%
    mutate(
      Starttime = as_datetime(Starttime),
      Stoptime = as_datetime(Stoptime),
      Gender = factor(Gender, levels = c("Unknown", "Male", "Female"))
    )
)

# daily ridership
summ.bikes <- bike.trips.df %>%
  mutate(Starttime = as.Date(Starttime)) %>% 
  filter(Starttime >= as.Date('2020-01-01')) %>% 
  count(Starttime) %>%
  rename(date = Starttime,
         bike_count = n)

# write out the file
# left_join(summ.ts, summ.bikes) %>% 
#   mutate(value = value / 1e6, 
#          bike_count = bike_count / 1e3,
#          text = format(date, "%b %d")) %>% 
#   write_csv('subway_citibike.csv')
  


# fix unemp data ----------------------------------------------------------

unemployment <- read_csv("~/Dropbox/Data/Projects/blog/static/d3/covid-impact/data/unemployment.csv")

# single file
# unemployment %>% 
#   # expand from weeks to days
#   full_join(tibble(DATE = seq(as.Date("2020-01-01"), as.Date("2020-03-28"), by = 1))) %>% 
#   arrange(DATE) %>% 
#   filter(DATE != as.Date("2020-03-28")) %>% 
#   mutate(ICSA = ICSA / 1e5,
#          text = format(DATE, "%b %d")) %>% 
#   write_csv('unemp.csv')

# all data
unemployment <- unemployment %>% 
  arrange(DATE) %>% 
  rename(date = DATE)

# unemployment %>% 
#   right_join(left_join(summ.ts, summ.bikes)) %>% 
#   mutate(value = value / 1e6, 
#          bike_count = bike_count / 1e3,
#          ICSA = ICSA / 1e6,
#          text = format(date, "%b %d")) %>% 
#   write_csv('subway_citi_unemp.csv')



# fix flight data ---------------------------------------------------------

flights <- read_csv("~/Dropbox/Data/Projects/blog/static/d3/covid-impact/data/number-of-commercial-fli.csv") %>% 
  select(date = DateTime, 
         flights = `Number of flights`)

# all data
# flights %>% 
#   # add random data as filler for days missing
#   full_join(tibble(date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = 1),
#                    flights = runif(5, min = 100000, max = 124000))) %>% 
#   arrange(date) %>% 
#   filter(date < as.Date("2020-03-28")) %>% 
#   right_join(right_join(unemployment, left_join(summ.ts, summ.bikes))) %>% 
#   mutate(value = value / 1e6, 
#          bike_count = bike_count / 1e3,
#          ICSA = ICSA / 1e6,
#          flights = flights / 1e3,
#          text = format(date, "%b %d")) %>% 
#   write_csv('sub_citi_unemp_flights.csv')



# combine all data and write out ------------------------------------------

# interpolate the unemployment data
unemployment <- unemployment %>% 
  # add random data as filler for days missing
  full_join(tibble(date = seq(as.Date("2020-01-04"), as.Date("2020-03-28"), by = 1))) %>% 
  arrange(date) %>% 
  mutate(ICSA_exists = !is.na(ICSA),
         ICSA = approx(x = date, y = ICSA, n = n())$y)
  

# gather the data to one data frame
summ.ts %>% 
  filter(date >= as.Date("2020-01-01"),
         date <= as.Date("2020-03-31")) %>% 
  left_join(summ.bikes) %>% 
  left_join(unemployment) %>% 
  left_join(flights) %>% 
  mutate(value = value / 1e6, 
         bike_count = bike_count / 1e3,
         ICSA = ICSA / 1e6,
         flights = flights / 1e3,
         text = format(date, "%b %d")) %>% 
  # mutate_all(as.character) %>% 
  # mutate_all(replace_na, replace = 0) %>% 
  write_csv('sub_citi_unemp_flights.csv')



# 2019 + 2020 -------------------------------------------------------------


# dates to filter based on SQL date format
dates.to.keep <- tibble(Date = 17857:20000) %>% 
  mutate(Formatted = as.Date(Date, origin = "1970-01-01")) %>% 
  filter(month(Formatted) %in% 1:4,
         year(Formatted) %in% 2019:2020) %>% 
  pull(Date)


# read all daily ridership into memory for the first 4 months of 2019 and 2020
turnstile.df <- c()
tables <- c("turnstile.2019", "turnstile.2020") # dbListTables(conn) %>% str_subset("turnstile*")
for (i in tables) {
  turnstile.df <- tbl(conn, i) %>%
    filter(Desc == "REGULAR" | Desc == "RECOVR AUD",
           Date %in% dates.to.keep) %>% 
    # calc entries by booth & SCP
    group_by(Booth, SCP) %>%
    mutate(Entries = Entries - lag(Entries)) %>% 
    ungroup() %>% 
    # trim outliers
    filter(Entries > 0,
           Entries <= 100000) %>% 
    group_by(Date) %>%
    summarize(Daily.ridership = sum(Entries)) %>% 
    select(Date, Daily.ridership) %>%
    collect() %>%
    mutate(Date = as.Date(Date, origin = "1970-01-01")) %>%
    na.omit() %>% 
    bind_rows(turnstile.df, .)
  gc()
}

turnstile.df %>%
  mutate(Year = as.factor(year(Date)),
         Date = as.Date(paste0(month(Date), "-", day(Date)), format = "%m-%d")) %>% 
  # remove outliers
  filter(Daily.ridership < 30000000) %>% 
  ggplot(aes(x = Date, y = Daily.ridership, group = Year, color = Year, alpha = Year)) +
  geom_line() +
  geom_point() +
  scale_alpha_manual(values = c(0.3, 1)) +
  scale_color_manual(values = c("grey70", "grey40")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  labs(title = 'Daily ridership drops significantly in March 2020',
       x = NULL,
       y = 'Daily ridership') +
  light.theme +
  theme(legend.title = element_blank())

