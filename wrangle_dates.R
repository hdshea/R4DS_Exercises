#' ---
#' title: "R4DS Wrangle Section: Dates Chapter"
#' author: "H. David Shea"
#' date: "22 January 2021"
#' output: github_document
#' ---
#'
#+ r setup, include = FALSE
library(tidyverse)
library(lubridate)
library(nycflights13)
#+

#' ## 16.2 creating date/times
flights %>% 
    select(year, month, day, hour, minute) %>% 
    mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
    make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
    filter(!is.na(dep_time), !is.na(arr_time)) %>% 
    mutate(
        dep_time = make_datetime_100(year, month, day, dep_time),
        arr_time = make_datetime_100(year, month, day, arr_time),
        sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
        sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
    ) %>% 
    select(origin, dest, ends_with("delay"), ends_with("time"))
#+ r dis1, include = FALSE
rm(make_datetime_100)
#+

flights_dt

flights_dt %>% 
    ggplot(aes(dep_time)) + 
    geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>% 
    filter(dep_time >= ymd(20130613), dep_time < ymd(20130615)) %>% 
    ggplot(aes(dep_time)) + 
    geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

#' ### 16.2 Exercises
#' 
#' Use the appropriate lubridate function to parse each of the following dates:
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
#'
mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)
#+ r dis2, include = FALSE
rm(d1,d2,d3,d4,d5)
#+

#' ## 16.3 date/times components
flights_dt %>% 
    mutate(wday = wday(dep_time, label = TRUE)) %>% 
    ggplot(aes(x = wday)) +
    geom_bar()

flights_dt %>% 
    select(dep_time) %>%
    mutate(dep_week = floor_date(dep_time, "week"))

#' ### 16.3 exercises
#' 
#' How does the distribution of flight times within a day change over the course of 
#' the year?
#' 
flights_dt %>% 
    mutate(
        sched_dep_hour = hour(sched_dep_time),
        sched_dep_month= month(sched_dep_time, label = TRUE)) %>%
    group_by(sched_dep_month, sched_dep_hour) %>%
    count() %>%
    ggplot(mapping = aes(x = sched_dep_month, y = sched_dep_hour)) +
    geom_tile(mapping = aes(fill = n))

#' Compare dep_time, sched_dep_time and dep_delay. Are they consistent? 
#' Explain your findings.
#' 
flights_dt %>% 
    mutate(
        calc_delay = as.numeric(dep_time - sched_dep_time) / 60,
        delay_diff = calc_delay - dep_delay
        ) %>%
    select(dep_time, sched_dep_time, dep_delay, calc_delay, delay_diff) %>%
    filter(delay_diff > 0)

#' Compare air_time with the duration between the departure and arrival. 
#' Explain your findings. (Hint: consider the location of the airport.)
#' 
flights_dt %>% 
    mutate(
        arr_dep_diff = as.numeric(arr_time - dep_time),
        var_ = abs(air_time - arr_dep_diff)
        ) %>%
    select(-contains("sched")) %>%
    left_join(
        select(mutate(airports, tz_dst = tz), faa, tz_dst), 
        by = c("dest" = "faa")
    ) %>%
    left_join(
        select(mutate(airports, tz_arr = tz), faa, tz_arr), 
        by = c("origin" = "faa")
    ) %>%
    filter(tz_dst != tz_arr) %>%
    select(-contains("sched"), -contains("_delay"), -dep_time, -arr_time)
#' large variations seem to be timezone differences between origin and dest 
#' but there are still unexplained variations
#' 

#' How does the average delay time change over the course of a day? 
#' Should you use dep_time or sched_dep_time? Why?
#' 
flights_dt %>%
    mutate(sched_dep_time = hour(sched_dep_time)) %>% # lump into hours through day
    group_by(sched_dep_time) %>%
    summarise(dep_delay = mean(dep_delay)) %>%
    ggplot(aes(y = dep_delay, x = sched_dep_time)) +
    geom_point()
    
#' On what day of the week should you leave if you want to minimise the chance of a 
#' delay?
#' 
flights_dt %>%
    mutate(sched_dep_day = wday(sched_dep_time, label = TRUE)) %>%
    group_by(sched_dep_day) %>%
    summarise(dep_delay = mean(dep_delay)) %>%
    ggplot(aes(y = dep_delay, x = sched_dep_day)) +
    geom_point()
#' Saturday
#' 

#' What makes the distribution of carat in diamond data and sched_dep_time in flights data similar?
#'
diamonds %>%
    ggplot(aes(carat)) +
    geom_density()

flights_dt %>%
    ggplot(aes(minute(sched_dep_time))) +
    geom_density()
#' groupings of high density at "regular' intervals: 1/2 carat, 1 carat, on half hour, on hour
#' 

#' Confirm my hypothesis that the early departures of flights in minutes 20-30 and 
#' 50-60 are caused by scheduled flights that leave early. Hint: create a binary 
#' variable that tells you whether or not a flight was delayed.
#' 
flights_dt %>%
    select( origin, dest, contains("dep_")) %>%
    mutate(
        delayed = dep_delay > 0,
        dep_min = minute(dep_time)
        ) %>%
    group_by(dep_min) %>%
    summarise(delayed = mean(delayed, na.rm = TRUE)) %>%
    ggplot(aes(dep_min, delayed)) +
    geom_line()
#' troughs in number of delays from 20-30 and 50-60 confirmed

#' ## 16.4 time spans
#' 
#' durations
ddays(1) == days(1)
#' 1 day period does equal 1 day duration
#' 

#' math calls for subtly on, for instance, leap years and time zone changes (e.g., EST to EDT)
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)
ymd_hms("2016-03-12 13:00:00", tz = "America/New_York") + ddays(1)
ymd_hms("2016-03-12 13:00:00", tz = "America/New_York") + days(1)

dyears(1) / ddays(365)
years(1) / days(1)

#' 6.4.5 Exercises
#' 
#' Create a vector of dates giving the first day of every month in 2015. 
make_date(2105, 1:12, 1)
#' Create a vector of dates giving the first day of every month in the current year.
make_date(year(now()), 1:12, 1)
 
#' Write a function that given your birthday (as a date), returns how old you are in years.
age_from_bday <- function(bday) {
    round((today() - bday) / dyears(1), 1)
}
age_from_bday(make_date(1962,2,14))
#+ r dis3, include = FALSE
rm(age_from_bday,flights_dt)
#+
