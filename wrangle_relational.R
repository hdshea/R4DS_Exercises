#' ---
#' title: "R4DS Wrangle Section: Relational Data Chapter"
#' author: "H. David Shea"
#' date: "18 January 2021"
#' output: github_document
#' ---
#'
library(tidyverse)
library(nycflights13)

##' example tables
#' 
flights
airlines
airports
planes
weather

#' exercises 13.2
#' 
#' Imagine you wanted to draw (approximately) the route each plane flies from its 
#' origin to its destination. What variables would you need? What tables would you
#' need to combine?
#' 
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

#' Very cute graphics added to this answer adapted from jrnold.github.io
#' 
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

#' primary keys 
#' 

#' testing uniqueness of primary keys
#' 
planes %>% 
    count(tailnum) %>% 
    filter(n > 1)

weather %>% 
    count(year, month, day, hour, origin) %>% 
    filter(n > 1)
#' There maybe erroneous entries in the weather table for 3 Nov 2013 at 1:00AM
#' 

flights %>% 
    count(year, month, day, carrier, flight) %>% 
    filter(n > 1)
#' Looks like Southwest and United may have reused flight numbers on some days - or some data problem
#' 
flights %>% 
    count(year, month, day, tailnum) %>% 
    filter(n > 1)
#' tailnum doesn't work in primary key as planes could fly circuits through the day
#' 

#' exercises 13.2
#' 
#' Add a surrogate key to flights.
#' 
flights_keyed <- flights %>%
    arrange(year, month, day, carrier, flight, sched_dep_time) %>%
    group_by(year, month, day, carrier, flight) %>%
    mutate(day_uid = row_number())

flights_keyed

flights_keyed %>%
    count(year, month, day, carrier, flight, day_uid) %>% 
    filter(n > 1)
