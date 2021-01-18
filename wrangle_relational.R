### R4DS Wrangle Section
library(tidyverse)
library(nycflights13)

# relational data

## example tables
flights
airlines
airports
planes
weather

# exercises 13.2

# Imagine you wanted to draw (approximately) the route each plane flies from its 
# origin to its destination. What variables would you need? What tables would you
# need to combine?
select(
    flights %>%
        filter(!is.na(arr_time)) %>% # flights that actually arrived at the destination
        inner_join(
            select(airports, origin = faa, origin_lat = lat, origin_lon = lon),
            by = "origin"
        ) %>%
        inner_join(
            select(airports, dest = faa, dest_lat = lat, dest_lon = lon),
            by = "dest"
        ),
    year, month, day, carrier, flight,
    origin, origin_lat, origin_lon,
    dest, dest_lat, dest_lon
)

# very cute - from jrnold.github.io
flights_latlon <-
    select(
        flights %>%
            filter(!is.na(arr_time)) %>% # flights that actually arrived at the destination
            inner_join(
                select(airports, origin = faa, origin_lat = lat, origin_lon = lon),
                by = "origin"
            ) %>%
            inner_join(
                select(airports, dest = faa, dest_lat = lat, dest_lon = lon),
                by = "dest"
            ),
        year, month, day, carrier, flight,
        origin, origin_lat, origin_lon,
        dest, dest_lat, dest_lon
    )
    
flights_latlon %>%
    slice(1:100) %>%
    ggplot(aes(
        x = origin_lon, xend = dest_lon,
        y = origin_lat, yend = dest_lat
    )) +
    borders("state") +
    geom_segment(arrow = arrow(length = unit(0.1, "cm"))) +
    coord_quickmap() +
    labs(y = "Latitude", x = "Longitude")

# keys 

# unique primary
planes %>% 
    count(tailnum) %>% 
    filter(n > 1)

weather %>% 
    count(year, month, day, hour, origin) %>% 
    filter(n > 1)
# erroneous entries in weather for 3 Nov 2013 at 1:00AM??

