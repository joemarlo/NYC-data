library(tidyverse)

# https://www.census.gov/programs-surveys/acs/data/pums.html
# 2018 5 year household NY file
psam_h36 <- read_csv("Analyses/COVID-neighborhoods/Data/psam_h36.csv", 
                     col_types = cols(MHP = col_double()))


# ADJINC adjustment factor for income; six decimals
# WGTP
# FINCP income
# GRPIP  rent as % of income
# HINCP household income
# SMARTPHONE 1 = yes
# JWDP time of departure for work
# INDP industry
# JWTR means of transportation to work: 04 subway or elevated
# The most detailed unit of geography contained in the PUMS files is the
  # Public Use Microdata Area (PUMA).
  # PUMA: https://www.census.gov/programs-surveys/geography/guidance/geo-areas/pumas.html
# https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_geography_handbook_2020.pdf



# mapping -----------------------------------------------------------------

# download nyc map shapes and clean them up
nyc.geojson <- httr::GET('https://data.cityofnewyork.us/api/geospatial/cwiz-gcty?method=export&format=GeoJSON')
nyc.neighborhoods <- rgdal::readOGR(httr::content(nyc.geojson, 'text'), 'OGRGeoJSON', verbose = FALSE)
# summary(nyc.neighborhoods)
nyc_PUMA_df <- broom::tidy(nyc.neighborhoods)
rm(nyc.geojson, nyc.neighborhoods)

# read in PUMA codes and ASSUME code order matches geojson file
# https://data.cityofnewyork.us/Housing-Development/Public-Use-Microdata-Areas-PUMA-/cwiz-gcty
nyc_PUMA_codes <- read_csv("Analyses/COVID-neighborhoods/Data/nypuma.csv")
nyc_PUMA_codes <- nyc_PUMA_codes %>% 
  select(PUMA) %>% 
  mutate(PUMA = paste0('0', as.character(PUMA)),
         id = as.character(0:(nrow(.)-1)))

nyc_PUMA_df <- left_join(nyc_PUMA_df, nyc_PUMA_codes, by = "id")

# map of nyc PUMAs
ggplot(nyc_PUMA_df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = PUMA),
               color = 'white') +
  scale_fill_discrete(type = replicate(55, paste0("#", paste0(sample(0:9, 6, TRUE), collapse = '')))) +
  coord_quickmap(xlim = c(-74.04, -73.79),
                 ylim = c(40.57, 40.88))
# PUMAs should match this map: https://www1.nyc.gov/assets/planning/download/pdf/data-maps/nyc-population/census2010/puma_cd_map.pdf

# map of nyc PUMAs by unweighted income
psam_h36 %>% 
  filter(PUMA %in% nyc_PUMA_codes$PUMA) %>% 
  group_by(PUMA) %>% 
  summarize(mean_income = mean(HINCP * (ADJINC / 1000000), na.rm = TRUE),
            .groups = 'drop') %>%
  right_join(nyc_PUMA_df, by = "PUMA") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean_income),
               color = 'white') +
  coord_quickmap(xlim = c(-74.04, -73.79),
                 ylim = c(40.57, 40.88)) +
  scale_fill_continuous(labels = scales::dollar_format(),
                        name = "Mean household income")

# map of nyc PUMAs by most common industry
psam_h36 %>% 
  filter(PUMA %in% nyc_PUMA_codes$PUMA) %>% 
  group_by(PUMA) %>% 
  summarize(mean_rent = mean(GRPIP / 100, na.rm = TRUE),
            .groups = 'drop') %>%
  right_join(nyc_PUMA_df, by = "PUMA") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean_rent),
               color = 'white') +
  coord_quickmap(xlim = c(-74.04, -73.79),
                 ylim = c(40.57, 40.88)) +
  scale_fill_continuous(labels = scales::percent_format(),
                        name = "Rent as % of income",
                        trans = 'reverse')

# map of nyc PUMAs by smartphone ownership
psam_h36 %>% 
  filter(PUMA %in% nyc_PUMA_codes$PUMA) %>% 
  group_by(PUMA) %>% 
  summarize(mean_SP = mean(abs(SMARTPHONE - 2), na.rm = TRUE),
            .groups = 'drop') %>%
  right_join(nyc_PUMA_df, by = "PUMA") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean_SP),
               color = 'white') +
  coord_quickmap(xlim = c(-74.04, -73.79),
                 ylim = c(40.57, 40.88)) +
  scale_fill_continuous(labels = scales::percent_format(),
                        name = "Smartphone ownership")

