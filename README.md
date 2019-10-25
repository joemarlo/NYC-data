# NYC-data

Individual files to clean and analyze the data are in the folders: [Citi-bike](Citi-bike), [Subway-turnstiles](Subway-turnstiles), [Taxi](Taxi):

- `Citi-bike.R`
- `Subway_turnstiles.R`
- `Taxi.R`

`Create_database.R`: creates a database of the Citi-bike, Subway, and (eventually) the Taxi data. Shell scripts in each folder must be run first to download the data

Once the database is created, data can easily be accessed via SQL and [dbplyr](https://dbplyr.tidyverse.org/) queries:
```
# establish the connections to the database
conn <- dbConnect(RSQLite::SQLite(), "NYC.db")

# query on disk
# turnstile.df <- tbl(conn, "turnstile.2019.09")
# turnstile.df %>%
#   select(Station, Time, Entries, Exits) %>%
#   group_by(Station) %>%
#   summarize(Entries = sum(Entries),
#             Exits = sum(Exits))

# or pulled into memory
turnstile.df <- tbl(conn, "turnstile.2019.09") %>% as_tibble() 
```

</br>

### Preliminary visualizations

<p align="center">
<img src="Plots/Commuting_light.png" width=80%>
</p>

<p align="center">
<img src="Plots/Subway_time_map.svg" width=83%>
</p>


<p align="center">
<img src="Plots/subway_hourly.gif" width=57%>
</p>

<p align="center">
<img src="Plots/Subway_ridership.svg" width=80%>
</p>

