library(tidyverse)
library(hms)
library(lubridate)
library(RSQLite)
source('Plots/ggplot-theme.R')

# connect to database and read in data to memory --------------------------

# establish the connection to the database
conn <- dbConnect(RSQLite::SQLite(), "NYC.db")

# read in just September data into memory
turnstile.df <- tbl(conn, "turnstile.2019") %>%
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

# week of	2019-01-05 wasn't reported until 2019-01-12
# tibble(Date = seq(as.Date("2019-01-05"), as.Date("2019-01-12"), by = 1)) %>% 
#   mutate(Daily.ridership = pull(turnstile.df[turnstile.df$Date == as.Date("2019-01-12"), 'Daily.ridership']) / 8) %>% 
#   bind_rows(turnstile.df %>% filter(Date != as.Date("2019-01-12"))) %>% 
#   arrange(Date)

# remove outlier week and non-Jan-March dates
turnstile.df <- turnstile.df %>% 
  filter(Date != as.Date("2019-01-12"),
         month(Date) %in% 1:3) %>% 
  mutate(Month.Day = paste0(month(Date), '-', day(Date)))

# convert ridership to two column: one for 2019 and one for 2020
summ.ts <- turnstile.df %>% 
  filter(Date >= as.Date('2020-01-01')) %>% 
  left_join(
    turnstile.df %>% 
      filter(Date <= as.Date('2020-01-01')) %>% 
      select(-Date),
    by = "Month.Day"
    ) %>% 
  select(date = Date, subway_2020 = Daily.ridership.x, subway_2019 = Daily.ridership.y)


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


# 2019+2020 daily ridership
summ.bikes <- bike.trips.df %>%
  mutate(Starttime = as.Date(Starttime)) %>% 
  filter(Starttime >= as.Date('2019-01-01')) %>% 
  count(Starttime) %>%
  rename(date = Starttime,
         bike_count = n) %>% 
  mutate(Month.Day = paste0(month(date), '-', day(date)))


# convert ridership to two column: one for 2019 and one for 2020
summ.bikes <- summ.bikes %>% 
  filter(date >= as.Date('2020-01-01')) %>% 
  left_join(
    summ.bikes %>% 
      filter(date <= as.Date('2020-01-01')) %>% 
      select(-date),
    by = "Month.Day"
  ) %>% 
  select(date, bike_2020 = bike_count.x, bike_2019 = bike_count.y)



# daily ridership
# summ.bikes <- bike.trips.df %>%
#   mutate(Starttime = as.Date(Starttime)) %>% 
#   filter(Starttime >= as.Date('2020-01-01')) %>% 
#   count(Starttime) %>%
#   rename(date = Starttime,
#          bike_count = n)

# write out the file
# left_join(summ.ts, summ.bikes) %>% 
#   mutate(value = value / 1e6, 
#          bike_count = bike_count / 1e3,
#          text = format(date, "%b %d")) %>% 
#   write_csv('subway_citibike.csv')
  


# fix unemp data ----------------------------------------------------------

# library(tidyquant)

unemployment <- tidyquant::tq_get('ICSA', 
                                  get = "economic.data", 
                                  from = "2020-01-01") %>% 
  rename(ICSA = price)

# single file
# unemployment %>% 
#   # expand from weeks to days
#   full_join(tibble(DATE = seq(as.Date("2020-01-01"), as.Date("2020-03-28"), by = 1))) %>% 
#   arrange(DATE) %>% 
#   filter(DATE != as.Date("2020-03-28")) %>% 
#   mutate(ICSA = ICSA / 1e5,
#          text = format(DATE, "%b %d")) %>% 
#   write_csv('unemp.csv')


# unemployment %>% 
#   right_join(left_join(summ.ts, summ.bikes)) %>% 
#   mutate(value = value / 1e6, 
#          bike_count = bike_count / 1e3,
#          ICSA = ICSA / 1e6,
#          text = format(date, "%b %d")) %>% 
#   write_csv('subway_citi_unemp.csv')



# fix flight data ---------------------------------------------------------

flights <- read_csv("Analyses/COVID-impact/number-of-commercial-fli.csv") %>% 
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


# google location data ----------------------------------------------------

if (!file.exists("Analyses/COVID-impact/google_trends.csv")) {
  read_tsv("https://github.com/nacnudus/google-location-coronavirus/raw/master/2020-03-29-country.tsv") %>%
    filter(country_code == 'US',
           type == 'country') %>%
    write_csv("Analyses/COVID-impact/google_trends.csv")
}
g.trends <- read_csv('Analyses/COVID-impact/google_trends.csv') %>% 
  select(date, trend, category) %>% 
  pivot_wider(names_from = 'category', values_from = 'trend') %>% 
  rename(retail_rec = 'Retail & recreation',
         groc_pharm = "Grocery & pharmacy",
         transit = "Transit stations",
         parks = 'Parks',
         workplaces = 'Workplaces')



# 311 data ----------------------------------------------------------------

three11 <- read_csv('Analyses/COVID-impact/311_Service_Requests_from_2010_to_Present.csv') 
  
three11 <- three11 %>% 
  select(date = `Created Date`,
         type = `Complaint Type`,
         Descriptor,
         City) %>% 
  mutate(date = mdy_hms(date, tz = Sys.timezone()) %>% as.Date(),
         month = month(date),
         year = year(date)) %>% 
  filter(month %in% 1:4,
         year %in% 2019:2020) %>% 
  mutate(type = str_to_sentence(type))

# plot of social distancing
three11 %>% 
  filter(grepl('*distancing*', Descriptor, ignore.case = TRUE)) %>% 
  count(date) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  geom_point()


top.cats.by.change <- tibble(
  type = c(
    'Non-emergency police matter',
    'Consumer complaint',
    'Blocked driveway',
    'Abandoned vehicle',
    'Derelict vehicles',
    'Illegal parking',
    'For hire vehicle complaint',
    'General',
    'Noise - commercial',
    'Noise - street/sidewalk',
    'Rodent',
    'Street condition',
    'Lost property',
    'Panhandling',
    'School maintenance',
    'Taxi complaint',
    'Derelict bicycle'
  )
)

# filter to the top categories
summ.311 <- three11 %>% 
  inner_join(top.cats.by.change) %>% 
  select(date, type, year) %>% 
  count(date, type, year) %>% 
  mutate(Month.Day = paste0(month(date), "-", day(date))) %>% 
  group_split(type)

# convert complaints to two column: one for 2019 and one for 2020; grouped by complaint type
summ.311 <- map_dfc(summ.311, function(tbl){
  
  # pull and type name
  name <- tbl$type[[1]] %>% 
    str_replace_all("/", " ") %>% 
    str_replace_all("-", " ") %>% 
    str_squish() %>% 
    str_replace_all(" ", "_")
  
  wide.tbl <- tbl %>% 
    filter(date >= as.Date('2020-01-01'),
           date <= as.Date('2020-03-31')) %>% 
    # ensure all dates are included
    full_join(tibble(date = seq(as.Date("2020-01-01"), 
                                as.Date("2020-03-31"), 
                                by = 1))) %>% 
    replace_na(replace = list(n = 0)) %>% 
    mutate(Month.Day = paste0(month(date), "-", day(date))) %>% 
    left_join(
      tbl %>% 
        filter(date >= as.Date('2019-01-01'),
               date <= as.Date('2019-04-02')) %>% 
        select(year, n.y = n, Month.Day) %>% 
        mutate(n = lead(n.y, n = 2)) %>% 
        select(-n.y), # move 2019 data two days forward to match weekdays
      by = "Month.Day"
    ) %>% 
    select(date, n.x, n.y) %>% 
    arrange(date)

  # clean up names
  names(wide.tbl) <- c("date", 
                       paste0(name, '_2020'),
                       paste0(name, '_2019'))
  
  return(wide.tbl)
}) %>% 
  select(-matches('date[1-9]'))

# remove extra date columns
summ.311 %>% 
  mutate(text = format(date, "%b %d")) %>% 
  write_csv('Analyses/COVID-impact/threeOneOne.csv')

var.names <- colnames(summ.311)[-1]

# names for d3
paste0(var.names, " : d.", var.names, ",", collapse =  " ")

# names for html
no.year.names <- str_sub(var.names, end = -6) %>% unique() %>% sort()
clean.names <- str_replace_all(no.year.names, "_", " ")
paste0('<a onclick="update311(\'', no.year.names, "'); changeText311('", clean.names, "')\">", clean.names, "</a>") %>% 
  writeLines()

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
  left_join(g.trends) %>% 
  mutate(subway_2020 = subway_2020 / 1e6, 
         subway_2019 = subway_2019 / 1e6,
         bike_2020 = bike_2020 / 1e3,
         bike_2019 = bike_2019 / 1e3,
         ICSA = ICSA / 1e6,
         flights = flights / 1e3,
         text = format(date, "%b %d")) %>% 
  write_csv('Analyses/COVID-impact/sub_citi_unemp_flights.csv')



