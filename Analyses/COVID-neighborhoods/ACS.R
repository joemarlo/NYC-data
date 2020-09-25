source("Analyses/COVID-neighborhoods/helper_functions.R")

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

# read in PUMA codes and ASSUME code order matches geojson file
# https://data.cityofnewyork.us/Housing-Development/Public-Use-Microdata-Areas-PUMA-/cwiz-gcty
nyc_PUMA_codes <- read_csv("Analyses/COVID-neighborhoods/Data/nypuma.csv")
nyc_PUMA_codes <- nyc_PUMA_codes %>% 
  select(PUMA, Borough) %>% 
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

# same map as previous but colored by Borough
ggplot(nyc_PUMA_df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Borough),
               color = 'white') +
  coord_quickmap(xlim = c(-74.04, -73.79),
                 ylim = c(40.57, 40.88))

# map of nyc PUMAs by weighted income
psam_h36 %>% 
  filter(PUMA %in% nyc_PUMA_codes$PUMA) %>% 
  group_by(PUMA) %>% 
  summarize(mean_income = sum(HINCP * ADJINC / 1000000 * WGTP, na.rm = TRUE) / sum(WGTP),
            .groups = 'drop') %>%
  right_join(nyc_PUMA_df, by = "PUMA") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean_income),
               color = 'white') +
  coord_quickmap(xlim = c(-74.04, -73.79),
                 ylim = c(40.57, 40.88)) +
  scale_fill_continuous(labels = scales::dollar_format(),
                        name = "Mean household income")

# map of nyc PUMAs by rent as % of income
psam_h36 %>% 
  filter(PUMA %in% nyc_PUMA_codes$PUMA) %>% 
  group_by(PUMA) %>% 
  summarize(mean_rent = sum((GRPIP / 100) * WGTP, na.rm = TRUE) / sum(WGTP),
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

# map of nyc PUMAs by smartphone ownership (unweighted)
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


# which stations are in each PUMA -----------------------------------------

# test the in polygon method
turnstile.df %>% 
  filter(Station == 'BEDFORD AV') %>% 
  head(1) %>% 
  ggplot(aes(x = Long, y = Lat)) +
  geom_polygon(data = nyc.df,
               aes(x = long, y = lat, group = group),
               fill = "gray50") +
  geom_point(color = 'grey20') +
  coord_quickmap(xlim = c(-74.05, -73.9),
                 ylim = c(40.65, 40.82))

test_point <- turnstile.df %>% 
  ungroup() %>% 
  filter(Station == 'BEDFORD AV') %>% 
  head(1) %>% 
  select(Lat, Long)

test_poly <- nyc_PUMA_df %>% 
  filter(PUMA == '04001')

# test if the point is the polygon
sp::point.in.polygon(point.x = test_point$Long, point.y = test_point$Lat,
                     pol.x = test_poly$long, pol.y = test_poly$lat)

# create a df with each row as a PUMA with a nest df of the polygon xy vectors
nested_polys <- nyc_PUMA_df %>% 
  select(long, lat, PUMA) %>% 
  group_by(PUMA) %>% 
  nest()

# create a df of just the station lat longs
all_points <- turnstile.df %>% 
  ungroup() %>% 
  select(Lat, Long) %>% 
  distinct()

# iterate through all the lat longs and check to see if a given lat long falls
  # inside each of the PUMA polygons
points_in_poly <- map2_dfr(.x = all_points$Long, .y = all_points$Lat, 
                           .f = function(x, y){
    
  df <- map2_dfr(.x = nested_polys$PUMA, .y = nested_polys$data,
                 .f = function(PUMA, poly) {
                   
                   result <- sp::point.in.polygon(
                     point.x = x, point.y = y,
                     pol.x = poly$long, pol.y = poly$lat
                     )
      
      return(tibble(PUMA = PUMA, in_poly = result))
    }
  )
  df$Long <- x
  df$Lat <- y
  return(df)
})

# create dataframe of points with their matching PUMA
point_poly_df <- points_in_poly %>% 
  filter(in_poly == 1) %>% 
  select(station_long = Long,
         station_lat = Lat,
         PUMA)

rm(nested_polys, all_points, points_in_poly, test_point, test_poly)


# calculate ridership decline by PUMA
ridership_drop_by_PUMA <- turnstile.df %>% 
  group_by(Date, Lat, Long) %>% 
  summarize(Daily.ridership = sum(Daily.ridership),
            .groups = 'drop') %>%
  filter(Date >= as.Date("2020-01-01"),
         Date <= as.Date("2020-06-14")) %>% 
  mutate(pre_covid = Date <= as.Date("2020-03-09"),
         post_covid = Date >= as.Date("2020-04-06")) %>% 
  filter(pre_covid | post_covid) %>%
  left_join(point_poly_df, by = c("Lat" = "station_lat", "Long" = "station_long")) %>% 
  group_by(pre_covid, PUMA, Date) %>% 
  summarize(total_ridership = sum(Daily.ridership, na.rm = TRUE),
            .groups = 'drop') %>%
  group_by(pre_covid, PUMA) %>% 
  summarize(total_ridership = mean(total_ridership),
            .groups = 'drop') %>% 
  group_by(PUMA) %>%
  arrange(pre_covid) %>% 
  mutate(ridership_change = total_ridership / lead(total_ridership) - 1) %>% 
  ungroup() %>% 
  na.omit() %>% 
  # filter(ridership_change > stats::quantile(ridership_change, probs = 0.025),
         # ridership_change < stats::quantile(ridership_change, probs = 0.975)) %>% 
  select(PUMA, ridership_change)

# qplot(ridership_drop_by_PUMA$ridership_change, geom = 'density')

# replicate the voronoi map by group by PUMA instead
ridership_drop_by_PUMA %>% 
  right_join(nyc_PUMA_df, by = "PUMA") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = ridership_change),
               color = 'white') +
  geom_point(data = point_poly_df,
             aes(x = station_long, y = station_lat), 
             color = 'grey90', size = 0.4, alpha = 0.9) +
  coord_quickmap(xlim = c(-74.04, -73.79),
                 ylim = c(40.57, 40.88)) +
  scale_fill_continuous(type = 'viridis', 
                        name = "Change in station entries\n",
                        labels = scales::percent_format(accuracy = 1),
                        breaks = c(-0.90, -0.80)) +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
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
# ggsave(filename = "Plots/change_in_ridership_by_PUMA.png",
#        device = 'png',
#        height = 10,
#        width = 5.5)


# income vs ridership
# first calc point estimate for income by PUMA
income_pt_est <- psam_h36 %>% 
  filter(PUMA %in% nyc_PUMA_codes$PUMA) %>% 
  group_by(PUMA) %>% 
  summarize(mean_income = sum(HINCP * ADJINC / 1000000 * WGTP, na.rm = TRUE) / sum(WGTP),
            .groups = 'drop')

# calc standard error for income by PUMA
  # and then plot against ridership change
psam_h36 %>% 
  filter(PUMA %in% nyc_PUMA_codes$PUMA) %>% 
  select(PUMA, HINCP, starts_with("WGTP")) %>% 
  rename(Weight = WGTP) %>% 
  pivot_longer(cols = starts_with("WGTP")) %>% 
  group_by(PUMA) %>% 
  summarize(variance = (4/80) * sum(value - Weight)^2,
            .groups = 'drop') %>% 
  mutate(conf_int = 1.96 * sqrt(variance)) %>% 
  left_join(income_pt_est, by = 'PUMA') %>% 
  mutate(low = mean_income - conf_int, high = mean_income + conf_int) %>% 
  left_join(ridership_drop_by_PUMA, by = "PUMA") %>%
  left_join(distinct(nyc_PUMA_df[, c('PUMA', 'Borough')]), by = "PUMA") %>% 
  filter(Borough != "Staten Island") %>% 
  ggplot(aes(x = mean_income, y = ridership_change, color = Borough,
             xmin = low, xmax = high)) +
  geom_point() +
  geom_linerange() +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_discrete(name = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = 0))) +
  labs(title = "Mean household income vs. decline in subway ridership",
       subtitle = 'Data aggregated on the Public Use Microdata Area (PUMA) level\nRange represents 95% confidence interval of household income',
       caption = 'Jan 1-Mar 4 compared to Apr 6-Jun 14\nData: MTA turnstiles, American Community Survey',
       x = "Mean household income",
       y = "Ridership change") +
  theme(legend.position = 'bottom')
# ggsave(filename = "Plots/change_in_ridership_vs_income.png",
#        device = 'png',
#        height = 5,
#        width = 7)
