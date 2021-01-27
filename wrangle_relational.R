#' ---
#' title: "R4DS Wrangle Section: Relational Data Chapter"
#' author: "H. David Shea"
#' date: "18 January 2021"
#' output: github_document
#' ---
#'
library(tidyverse)
library(nycflights13)
library(Lahman)
library(nasaweather)

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

#' exercises 13.3
#' 
#' AddIdentify the keys in the following datasets:
#' 
#' 1. Lahman::Batting
#' 2. babynames::babynames
#' 3. nasaweather::atmos
#' 4. fueleconomy::vehicles
#' 5. ggplot2::diamonds
#' 

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
    select( uid, carat:z)

#' 13.4 mutating joins 
#' 
flights2 <- flights %>% 
    select(year:day, hour, origin, dest, tailnum, carrier)

#' left join
flights2 %>%
    select(-origin, -dest) %>% 
    left_join(airlines, by = "carrier")

#' equivalent to a mutate matching on carrier - but left_join is better reading
flights2 %>%
    select(-origin, -dest) %>% 
    mutate(name = airlines$name[match(carrier, airlines$carrier)])

#' 
#' join types:
#' 
#' inner_join - exact match on keys
#' 
#' outer join:
#' 
#' left_join - all obs from left (x in documentation) relation and matching from right (y in documentation)
#' 
#' right_join - all obs from right relation and matching from left
#' 
#' full_join - all obs from left and right with matching keys linked
#' 

#' exercises 13.4
#' 
#' Compute the average delay by destination, then join on the airports data frame 
#' so you can show the spatial distribution of delays.
#' 
flights %>%
    filter(!is.na(arr_delay)) %>%
    group_by(dest) %>%
    summarize(avg_arr_delay = mean(arr_delay)) %>%
    filter(!is.na(avg_arr_delay)) %>%
    inner_join(airports, c("dest" = "faa")) %>%
    ggplot(aes(lon, lat)) +
    borders("state") +
    geom_point(aes(color = avg_arr_delay), alpha = 4/5) +
    coord_quickmap()

#' Add the location of the origin and destination (i.e. the lat and lon) to flights.
#' 
flights %>%
    left_join(
        select(airports, faa, origin_lon=lon, origin_lat=lat),
        by = c("origin" = "faa")
        ) %>%
    left_join(
        select(airports, faa, dest_lon=lon, dest_lat=lat), 
        by = c("dest" = "faa"))

#' Is there a relationship between the age of a plane and its delays?
#' 
plane_age_summ <- flights %>%
    filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
    inner_join(
        select(planes, tailnum, first_year = year),
        by = "tailnum") %>%
    filter(!is.na(first_year)) %>%  # need to decide if you want to excluded these
    mutate(age = year-first_year) %>%
    select(tailnum, year:day, arr_delay, dep_delay, age) %>%
    group_by(age) %>%
    summarize(
        avg_dep_delay = mean(dep_delay),
        avg_arr_delay = mean(arr_delay)
    )
    
ggplot(plane_age_summ, aes(age,avg_dep_delay)) +
    geom_point()

ggplot(plane_age_summ, aes(age,avg_arr_delay)) +
    geom_point()
#' 
#' regardless of arrival or departure delay, there is some increasing positive
#' correlation between age and delay from 0 to 10 years, which then trails back down
#' 

#' What weather conditions make it more likely to see a delay?
#' 
delay_weather <- select(flights, origin, year, month, day, hour, dep_delay) %>%
    inner_join(weather,
              by = c(
                  "origin" = "origin", 
                  "year" = "year", 
                  "month" = "month", 
                  "day" = "day", 
                  "hour" = "hour"
              ))

#' visib and precip are pretty finite groups (20 and 59 distinct, respectively)
#' 
delay_weather %>%
    group_by(visib) %>%
    summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ggplot(aes(visib, dep_delay)) +
    geom_point() + 
    geom_smooth()
#' visib shows positive impact when low, flattening out after 2.5 miles, dropping down at ten miles
#' 

delay_weather %>%
    ungroup() %>%
    group_by(precip) %>%
    summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ggplot(aes(precip, dep_delay)) +
    geom_point() + 
    geom_smooth()
#' non-zero precip shows increasing positive trend with delay
#' 

#' for other variables, trying categorizing for smaller groups
#' 
delay_weather %>%
    ungroup() %>%
    mutate(temp = round(temp)) %>%
    group_by(temp) %>%
    summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ggplot(aes(temp, dep_delay)) +
    geom_point() + 
    geom_smooth()
#' temp shows a poitive trend with delay above ~50 degrees - maybe heavy volume summer months??

delay_weather %>%
    ungroup() %>%
    mutate(dewp = cut_width(dewp, 10)) %>%
    group_by(dewp) %>%
    summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ggplot(aes(dewp, dep_delay)) +
    geom_point()
#' dewp maybe increasing trend around 50 degrees - see temp note - possible correlation

delay_weather %>%
    ungroup() %>%
    mutate(humid = cut_width(humid, 10)) %>%
    group_by(humid) %>%
    summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ggplot(aes(humid, dep_delay)) +
    geom_point()
#' humid maybe increasing trend around 50 - see temp note - possible correlation

delay_weather %>%
    ungroup() %>%
    mutate(wind_dir = cut_width(wind_dir, 30, boundary = 0)) %>%
    group_by(wind_dir) %>%
    summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ggplot(aes(wind_dir, dep_delay)) +
    geom_point()
#' wind_dir no correlation

delay_weather %>%
    ungroup() %>%
    mutate(wind_speed = cut_width(wind_speed, 5, boundary = 0)) %>%
    group_by(wind_speed) %>%
    summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ggplot(aes(wind_speed, dep_delay)) +
    geom_point()
#' wind_speed slight positive correlation - needs more investigation

delay_weather %>%
    ungroup() %>%
    mutate(wind_gust = cut_width(wind_gust, 5, boundary = 0)) %>%
    group_by(wind_gust) %>%
    summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ggplot(aes(wind_gust, dep_delay)) +
    geom_point()
#' wind_gust no correlation

delay_weather %>%
    ungroup() %>%
    mutate(pressure = cut_width(pressure, 5, boundary = 0)) %>%
    group_by(pressure) %>%
    summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ggplot(aes(pressure, dep_delay)) +
    geom_point()
#' pressure no correlation

#' 
#' 
#' What happened on June 13 2013? Display the spatial pattern of delays, and then 
#' use Google to cross-reference with the weather.
#' 
nycweather_20130613 <- flights %>%
    filter(year == 2013, month == 6, day == 13) %>%
    group_by(origin, year, month, day, hour) %>%
    summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    inner_join(weather,
               by = c(
                   "origin" = "origin", 
                   "year" = "year", 
                   "month" = "month", 
                   "day" = "day", 
                   "hour" = "hour"
               )) %>%
    group_by(hour) %>%
    summarise(
        dep_delay = mean(dep_delay),
        temp = mean(temp),
        dewp = mean(dewp),
        humid = mean(humid),
        wind_speed = mean(wind_speed),
        precip = mean(precip)
    )

#' Storms broadly blew through the eastern / southeastern US - impacting NYC airports 
#' with increasing wind speed and humidity through the day.  Average delays at NYC airports
#' increased accordingly, compounding as each hour went by.
#' 
nycweather_20130613 %>%
    select(hour, dep_delay, humid, wind_speed)


#' 13.5 filtering joins 
#' 


#' exercises 13.5
#' 
#' What does it mean for a flight to have a missing tailnum? What do the tail 
#' numbers that don’t have a matching record in planes have in common? (Hint: 
#' one variable explains ~90% of the problems.)
#' 
flights %>% filter(is.na(tailnum), !is.na(dep_time))
#' every flight with a missing tailnum has no departure time - likely the planes
#' never showed up at the origination airport from a previous leg delay
#' 

flights %>% 
    anti_join(planes, by = "tailnum") %>%
    group_by(carrier) %>%
    count() %>%
    arrange(desc(n)) %>%
    inner_join(airlines)
#' most of the flights with missing tailnum are Envoy or American flights - documentation 
#' says these two airline's planes cannot be matched via tailnum
#' 

#' Filter flights to only show flights with planes that have flown at least 100 flights.
#' 
flights %>%
    semi_join(
        flights %>% group_by(tailnum) %>% count() %>% filter(n >= 100),
        by = "tailnum"
    )

#' Combine fueleconomy::vehicles and fueleconomy::common to find only the 
#' records for the most common models.
#' 
fueleconomy::vehicles %>% semi_join(fueleconomy::common)

#' Find the 48 hours (over the course of the whole year) that have the worst delays. 
#' Cross-reference it with the weather data. Can you see any patterns?
#' 
highest_delays <- flights %>%
    group_by(year, month, day, hour) %>%
    summarise(dep_delay = mean(dep_delay, rm.na = TRUE)) %>%
    arrange(desc(dep_delay)) %>%
    head(48) %>%
    inner_join(weather)

quantile(select(weather,wind_speed), na.rm = TRUE)
quantile(select(highest_delays,wind_speed), na.rm = TRUE)
highest_delays %>% group_by(month) %>% count()
#' winds are above average versus the overall average with the high end range much higher
#' 
#' and most of the delays by hour are in the summer:  Jun, Jul and Aug
#' 

#' What does anti_join(flights, airports, by = c("dest" = "faa")) tell 
#' you? What does anti_join(airports, flights, by = c("faa" = "dest")) 
#' tell you?
#' 
anti_join(flights, airports, by = c("dest" = "faa")) %>%
    group_by(dest) %>%
    count()
#' these destinations are outside of the 50 US States and not included in airports database
#' 
anti_join(airports, flights, by = c("faa" = "dest"))
#' in 2013, no flights from NYC airports had these airports as a desitnation
#' 

#' You might expect that there’s an implicit relationship between plane and airline, 
#' because each plane is flown by a single airline. Confirm or reject this hypothesis 
#' using the tools you’ve learned above.
flights %>%
    filter(!is.na(tailnum)) %>%
    distinct(tailnum, carrier) %>%
    group_by(tailnum) %>%
    filter(n() > 1) %>%
    left_join(airlines, by = "carrier") %>%
    arrange(tailnum, carrier) 
#' confirms that several planes (in the flights dataset) were owned/operated by two distinct carriers in 2013

