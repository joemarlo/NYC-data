require(tidyverse)
require(httr)
require(rgdal)
require(broom)

#theme for ggplot
seashell.theme <- theme(panel.grid.minor = element_line(color = NA),
                        panel.grid.major = element_line(color = "seashell"),
                        panel.background = element_rect(fill = "#f5e9e1"),
                        plot.background = element_rect(fill = "seashell",
                                                       color = NA),
                        axis.title = element_text(color = "gray30"),
                        axis.ticks = element_line(color = NA),
                        strip.background = element_rect(fill = NA),
                        strip.text = element_text(color = "gray30",
                                                  size = 11,
                                                  face = "bold"),
                        legend.background = element_rect(fill = "seashell"),
                        plot.title = element_text(color = "gray30",
                                                  face = "bold"),
                        plot.subtitle = element_text(size = 10),
                        text = element_text(family = "Courier"))

light.theme <- theme(panel.grid.minor.y = element_line(color = NA),
                     panel.grid.major.y = element_line(color = "gray95"),
                     panel.grid.minor.x = element_line(color = NA),
                     panel.grid.major.x = element_line(color = "gray95"),
                     panel.background = element_rect(fill = NA),
                     plot.background = element_rect(fill = NA,
                                                    color = "gray95",
                                                    size = 10),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     axis.title = element_text(color = "gray30"),
                     axis.ticks = element_line(color = NA),
                     strip.background = element_rect(fill = "gray95"),
                     strip.text = element_text(color = "gray30",
                                               size = 11,
                                               face = "bold"),
                     plot.title = element_text(color = "gray30",
                                               face = "bold"),
                     plot.subtitle = element_text(size = 10,
                                                  color = "gray30"),
                     text = element_text(family = "Helvetica"))

# download nyc map shapes and clean them up
nyc.geojson <- GET('https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON')
nyc.neighborhoods <- readOGR(httr::content(nyc.geojson,'text'), 'OGRGeoJSON', verbose = FALSE)
# summary(nyc.neighborhoods)
nyc.df <- tidy(nyc.neighborhoods)
rm(nyc.geojson, nyc.neighborhoods)
# id == 0 is the bronx
# id == 1 is staten island
# id == 2 is brooklyn
# id == 3 is queens
# id == 4 is manhattan
# check to see the shapefile works
# ggplot() +
#   geom_polygon(data = nyc.df, aes(x = long, y = lat, group = group), fill = "gray") +
#   coord_quickmap(xlim = c(-74.05, -73.9),
#                  ylim = c(40.65, 40.84))


# website color is #2b7551

# ggsave(filename = "LRC_winners.svg",
#        plot = last_plot(),
#        path = project.path,
#        device = "svg",
#        width = 9,
#        height = 5)
