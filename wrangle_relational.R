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

#' 
#' AddIdentify the keys in the following datasets:
#' 
#' 1. Lahman::Batting
#' 2. babynames::babynames
#' 3. nasaweather::atmos
#' 4. fueleconomy::vehicles
#' 5. ggplot2::diamonds
#' 
library(Lahman)
library(nasaweather)

#' 
#' Lahman::Batting
#' PlayerID, yearID, stint - stint identifies the order of appearance for a player who played on multiple teams in a year 
#' 
Batting %>%
    count(playerID, yearID, stint) %>% 
    filter(n > 1)

#' 
#' babynames::babynames
#' year, sex, name - 5 females named Arthur in 1880
#' 
babynames::babynames %>%
    count(year, sex, name) %>% 
    filter(n > 1)

#' 
#' nasaweather::atmos
#' lat, long, year, month
#' 
nasaweather::atmos %>%
    count(lat, long, year, month) %>% 
    filter(n > 1)

#' 
#' ggplot2::diamonds
#' 
#' options (depth is a function of x, y, and z):
#' 
#' carat, cut, color, clarity, depth
#' 
#' carat, cut, color, clarity, x, y, z
#' 
diamonds %>%
    count(carat, cut, color, clarity, depth) %>% 
    filter(n > 1)

diamonds %>%
    count(carat, cut, color, clarity, x, y, z) %>% 
    filter(n > 1)

#'
#' as neither option provides uniqueness - and because it probably makes sense in general - 
#' there is no primary key in diamonds and - if desired - should probably be created for each 
#' individual diamond
#' 
diamonds %>%
    mutate(uid = row_number()) %>%
    select( uid, carat, cut, color, clarity, depth, table, price, x, y, z)
