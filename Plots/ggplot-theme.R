require(tidyverse)

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