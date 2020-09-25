require(tidyverse)
require(viridis)

# build custom theme
theme_custom <- function() {
  theme_gray() +
    theme(
      panel.grid.minor.y = element_line(color = NA),
      panel.grid.major.y = element_line(color = "gray95"),
      panel.grid.minor.x = element_line(color = NA),
      panel.grid.major.x = element_line(color = "gray95"),
      panel.background = element_rect(fill = NA),
      plot.background = element_rect(
        fill = NA,
        color = "gray95",
        size = 10
      ),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      axis.title = element_text(color = "gray30"),
      axis.ticks = element_line(color = NA),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(
        color = "gray30",
        size = 11,
        face = "bold"
      ),
      plot.title = element_text(color = "gray30",
                                face = "bold"),
      plot.subtitle = element_text(size = 10,
                                   color = "gray30"),
      text = element_text(family = "Helvetica"),
      plot.caption = element_text(face = "italic",
                                  size = 6,
                                  color = 'grey50'),
      legend.key = element_rect(fill = NA)
    )
}

theme_set(theme_custom())

# set default continuous colors
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

save_plot <- function(filename, device = 'png', height = 4, width = 6, ...){
  # wrapper to ggsave with defaults
  ggsave(...,
         filename = paste0(filename, ".", device),
         device = device,
         height = height,
         width = width
  )
}



# download nyc map shapes and clean them up
nyc.geojson <- httr::GET('https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON')
nyc.neighborhoods <- rgdal::readOGR(httr::content(nyc.geojson,'text'), 'OGRGeoJSON', verbose = FALSE)
nyc.df <- broom::tidy(nyc.neighborhoods)
rm(nyc.geojson, nyc.neighborhoods)

# download nyc PUMA map shapes and clean them up
nyc.geojson <- httr::GET('https://data.cityofnewyork.us/api/geospatial/cwiz-gcty?method=export&format=GeoJSON')
nyc.neighborhoods <- rgdal::readOGR(httr::content(nyc.geojson, 'text'), 'OGRGeoJSON', verbose = FALSE)
nyc_PUMA_df <- broom::tidy(nyc.neighborhoods)
rm(nyc.geojson, nyc.neighborhoods)