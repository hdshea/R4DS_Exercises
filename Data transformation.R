library(nycflights13)
library(tidyverse)

flights

## filter

filter(flights, month == 1, day == 1)

# 5.2.4 Exercises
# Had an arrival delay of two hours or more
filter(flights, arr_delay >= 120)

# Flew to Houston (IAH or HOU)
filter(flights, dest == "IAH" | dest == "HOU")

# Were operated by United, American or Delta
filter(flights, carrier %in% c("AA", "DL", "UA"))

#Departed in summer (July, August, and September)
filter(flights, month %in% c(7, 8, 9))

# Arrived more than two hours late, but didn’t leave late
filter(flights, arr_delay >= 120, dep_delay <= 0)

# Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, arr_delay >= 60, dep_delay - arr_delay > 30)

# Departed between midnight and 6am (inclusive)
filter(flights, dep_time == 2400 | dep_time <= 600)

# Another useful dplyr filtering helper is between(). What does it do? Can you use 
# it to simplify the code needed to answer the previous challenges?
filter(flights, !between(dep_time, 601, 2359))

# How many flights have a missing dep_time? What other variables are missing?
# What might these rows represent?
filter(flights, is.na(dep_time))
# canceled flights

# Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA 
# not missing? Can you figure out the general rule? (NA * 0 is a tricky 
# counterexample!)
NA ^ 0 # EVERYTHING RAISED TO THE 0 POWER = 1
NA | TRUE # LOGICAL OR WITH TRUE ALWAYS TRUE
FALSE & NA # LOGICAL AND WITH FALSE ALWAYS FALSE
NA * 0
Inf * 0 # ???


## arrange

arrange(flights, year, month, day)

arrange(flights, desc(dep_delay))

# 5.3

# How could you use arrange() to sort all missing values to the start? (Hint: use is.na())
arrange(flights, desc(is.na(tailnum), tailnum))

# Sort flights to find the most delayed flights. Find the flights that left earliest.
arrange(flights, desc(dep_delay))
arrange(flights, dep_time)

# Sort flights to find the fastest (highest speed) flights.
select(arrange(flights, desc(distance / air_time)), origin:minute)

# Which flights traveled the farthest? Which traveled the shortest?
select(arrange(flights, desc(distance)), origin:minute)
select(arrange(flights, distance), origin:minute)


## select, rename
# 5.4

# Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, starts_with("dep_"), starts_with("arr_"))
select(flights, any_of(c("dep_time", "dep_delay", "arr_time", "arr_delay")))
select(flights, 4, 6, 7, 9)

# What happens if you include the name of a variable multiple times in a select() call?
select(flights, dep_time, dep_time)

# What does the any_of() function do? Why might it be helpful in conjunction with this vector?
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(vars))

# Does the result of running the following code surprise you? How do the select 
# helpers deal with case by default? How can you change that default?
select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = FALSE))


## mutate, transmute

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)

transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

(x <- 1:10)
lag(x)
lead(x)

x
cumsum(x)
cummean(x)

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

# 5.5

# Currently dep_time and sched_dep_time are convenient to look at, but hard to 
# compute with because they’re not really continuous numbers. Convert them to a 
# more convenient representation of number of minutes since midnight.
transmute(flights,
          dep_time = dep_time %% 2400,
          dep_mins = ((dep_time %/% 100) * 60) + dep_time %% 100,
          sched_dep_time = sched_dep_time %% 2400,
          sched_dep_mins = ((sched_dep_time %/% 100) * 60) + sched_dep_time %% 100
)

# Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? 
# What do you need to do to fix it?
transmute(flights, 
          air_time = air_time, 
          x_air_time = arr_time - dep_time
)
transmute(flights, 
          arr_time = arr_time %% 2400,
          dep_time = dep_time %% 2400,
          air_time = air_time, 
          x_air_time = (((arr_time %/% 100) * 60) + arr_time %% 100) - (((dep_time %/% 100) * 60) + dep_time %% 100),
)

# Compare dep_time, sched_dep_time, and dep_delay. How would you expect 
# those three numbers to be related?
transmute(flights, 
          dep_time = dep_time %% 2400,
          dep_mins = ((dep_time %/% 100) * 60) + dep_time %% 100,
          sched_dep_time = sched_dep_time %% 2400,
          sched_dep_mins = ((sched_dep_time %/% 100) * 60) + sched_dep_time %% 100,
          dep_delay,
          calc_dep_delay = dep_mins - sched_dep_mins
)

# Find the 10 most delayed flights using a ranking function. How do you want to 
# handle ties? Carefully read the documentation for min_rank().
select(
    arrange(
        filter(
            mutate(flights,delay_rank = min_rank(desc(arr_delay))),
            delay_rank <= 10),
        delay_rank),
    dep_delay, arr_delay:delay_rank
)
select(
    arrange(
        filter(
            mutate(flights,delay_rank = min_rank(desc(dep_delay))),
            delay_rank <= 10),
        delay_rank),
    dep_delay,arr_delay:delay_rank
)

# What does 1:3 + 1:10 return? Why?
c( 1+1, 2+2, 3+3, 1+4, 2+5, 3+6, 1+7, 2+8, 3+9, 1+10)
1:3 + 1:10

# What trigonometric functions does R provide?
cos(x)
sin(x)
tan(x)

acos(x)
asin(x)
atan(x)
atan2(y, x)

cospi(x)
sinpi(x)
tanpi(x)


## summarise, group_by

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

summarise(
    group_by(flights, year, month, day),
    delay = mean(dep_delay, na.rm = TRUE)
)

delay <- summarise(group_by(flights, dest),
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)

delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1/3) +
    geom_smooth(se = FALSE)

delays <- flights %>% 
    group_by(dest) %>% 
    summarise(
        count = n(),
        dist = mean(distance, na.rm = TRUE),
        delay = mean(arr_delay, na.rm = TRUE)
    ) %>% 
    filter(count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1/3) +
    geom_smooth(se = FALSE)

not_cancelled <- flights %>% 
    filter(!is.na(dep_delay), !is.na(arr_delay))

delays <- not_cancelled %>% 
    group_by(tailnum) %>% 
    summarise(
        delay = mean(arr_delay)
    )
#> `summarise()` ungrouping output (override with `.groups` argument)

ggplot(data = delays, mapping = aes(x = delay)) + 
    geom_freqpoly(binwidth = 10)

not_cancelled %>% 
    group_by(year, month, day) %>% 
    summarise(hour_prop = 100 * mean(arr_delay > 60))

daily <- group_by(flights, year, month, day)

summarise(daily, flights = n())
summarise(daily, flights = n())

(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

daily %>% 
    ungroup() %>%             # no longer grouped by date
    summarise(flights = n())  # all flights

# 5.6

# A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.
group_by(flights, carrier, flight) %>%
    summarise(
        early15 = mean(arr_delay <= -15, na.rm = TRUE),
        late15 = mean(arr_delay >= 15, na.rm = TRUE),
        n = n()) %>%
    filter(early15 >= 0.4, late15 >= 0.4)

# A flight is always 10 minutes late.
group_by(flights, carrier, flight) %>%
    summarise(
        late10 = mean(arr_delay >= 10, na.rm = TRUE),
        n = n()) %>%
    filter(late10 >= 0.99, n >= 3) %>%
    arrange(desc(n))

# A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.
group_by(flights, carrier, flight) %>%
    summarise(
        early30 = mean(arr_delay <= -30, na.rm = TRUE),
        late30 = mean(arr_delay >= 30, na.rm = TRUE),
        n = n()) %>%
    filter(early30 >= 0.5, late30 >= 0.5)

# 99% of the time a flight is on time. 1% of the time it’s 2 hours late.
filter(flights, !is.na(arr_delay)) %>%
    group_by(carrier, flight) %>%
    summarise(
        ontime_prop = mean(arr_delay <= 0),
        late_prop = mean(arr_delay > 0),
        n = n()) %>%
    filter(ontime_prop >= 0.99, late_prop > 0, n > 1)

# Which is more important: arrival delay or departure delay?
filter(flights, !is.na(arr_delay), is.na(dep_delay))
filter(flights, is.na(arr_delay), !is.na(dep_delay))
# arr_delay

# Come up with another approach that will give you the same output as 
# not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, 
# wt = distance) (without using count()).
not_cancelled <- flights %>% 
    filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% count(dest)
not_cancelled %>% group_by(dest) %>% summarise(n=n())

not_cancelled %>% count(tailnum, wt = distance)
not_cancelled %>% group_by(tailnum) %>% summarise(n=sum(distance))

# Look at the number of cancelled flights per day. Is there a pattern? Is the 
# proportion of cancelled flights related to the average delay?
x <- flights %>%
    group_by(year, month, day) %>%
    summarise(
        cancelled_prop = 100 * mean(is.na(arr_delay)),
        arr_delay_mean = mean(arr_delay, na.rm = TRUE),
        dep_delay_mean = mean(dep_delay, na.rm = TRUE)
        )

ggplot(data = x, mapping = aes(x = arr_delay_mean, y = cancelled_prop)) +
    geom_point()

ggplot(data = x, mapping = aes(x = dep_delay_mean, y = cancelled_prop)) +
    geom_point()

# Which carrier has the worst delays? Challenge: can you disentangle the effects of 
# bad airports vs. bad carriers? Why/why not? (Hint: think about flights %>% 
# group_by(carrier, dest) %>% summarise(n()))

flights %>%
    group_by(carrier) %>%
    summarise(
        arr_delay_mean = mean(arr_delay, na.rm = TRUE),
        dep_delay_mean = mean(dep_delay, na.rm = TRUE)
    ) %>%
    arrange(desc(arr_delay_mean))

filter(airlines, carrier %in% c("F9", "FL", "EV", "YV", "OO", "MQ"))
# EV      ExpressJet Airlines Inc.   
# F9      Frontier Airlines Inc.     
# FL      AirTran Airways Corporation
# MQ      Envoy Air                  
# OO      SkyWest Airlines Inc.      
# YV      Mesa Airlines Inc.       

flights %>% group_by(carrier, dest) %>% count(sort = TRUE)


## grouped mutates and filters

popular_dests <- flights %>% 
    group_by(dest) %>% 
    filter(n() > 365)
popular_dests

# 5.7

# Which plane (tailnum) has the worst on-time record?
flights %>%
    group_by(tailnum) %>%
    summarise(
        n = n(),
        arr_delay_mean = mean(arr_delay, na.rm = TRUE),
        arr_delay_prop = mean(!is.na(arr_delay) & (arr_delay > 0))
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(arr_delay_mean))

flights %>%
    group_by(tailnum) %>%
    summarise(
        n = n(),
        arr_delay_mean = mean(arr_delay, na.rm = TRUE),
        arr_delay_prop = mean(!is.na(arr_delay) & (arr_delay > 0))
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(arr_delay_prop))

# What time of day should you fly if you want to avoid delays as much as possible?

x <- flights %>%
    group_by(hour) %>%
    summarise(
        n = n(),
        arr_delay_mean = mean(arr_delay, na.rm = TRUE),
        arr_delay_prop = mean(!is.na(arr_delay) & (arr_delay > 0))
    ) %>%
    arrange(desc(arr_delay_prop))


ggplot(data = x, mapping = aes(x = hour, y = arr_delay_prop)) +
    geom_point()

# For each destination, compute the total minutes of delay. For each flight, compute
# the proportion of the total delay for its destination.

flights %>%
    filter(arr_delay > 0) %>%
    group_by(dest) %>%
    mutate(
        arr_delay_total = sum(arr_delay),
        arr_delay_prop = 100 * arr_delay / arr_delay_total
    ) %>%
    select(dest, month, day, dep_time, carrier, flight,
           arr_delay, arr_delay_prop) %>%
    arrange(dest, desc(arr_delay_prop))

# Look at each destination. Can you find flights that are suspiciously fast?
# (i.e. flights that represent a potential data entry error). Compute the air time of a 
# flight relative to the shortest flight to that destination. Which flights were most 
# delayed in the air?

flights %>%
    filter(!is.na(arr_delay), !is.na(distance), !is.na(air_time)) %>%
    group_by(origin, dest) %>%
    mutate(
        miles_per_hour = distance / (air_time / 60),
        mph_avg = mean(miles_per_hour),
        mph_prop = miles_per_hour / mph_avg
    ) %>%
    select(origin, dest, month, day, dep_time, carrier, flight, 
           distance, air_time, miles_per_hour, mph_prop) %>%
    filter(mph_prop < 0.5) %>%
    arrange(mph_prop)

standardized_flights <- flights %>%
    filter(!is.na(air_time)) %>%
    group_by(dest, origin) %>%
    mutate(
        air_time_mean = mean(air_time),
        air_time_sd = sd(air_time),
        n = n()
    ) %>%
    ungroup() %>%
    mutate(air_time_standard = (air_time - air_time_mean) / (air_time_sd + 1))

standardized_flights %>%
    arrange(air_time_standard) %>%
    select(
        carrier, flight, origin, dest, month, day,
        air_time, air_time_mean, air_time_standard
    ) %>%
    head(10) %>%
    print(width = Inf)

flights %>%
    filter(!is.na(distance)) %>%
    group_by(origin, dest) %>%
    summarise(
        adistance_avg = mean(distance),
        distance_std = sd(distance)
    ) %>%
    arrange(desc(distance_std))
filter(flights, origin == "EWR", dest == "EGE") %>% select( month, day, distance ) %>% arrange(desc(distance))
filter(flights, origin == "EWR", dest == "EGE") %>% select( month, day, distance ) %>% arrange(distance)

# Find all destinations that are flown by at least two carriers. Use that information to rank the carriers.
flights %>%
    group_by(dest) %>%
    mutate(n_carrier = n_distinct(carrier)) %>% 
    filter(n_carrier >= 2) %>%
    group_by(carrier) %>%
    summarise(n_dest = n_distinct(dest)) %>%
    arrange(desc(n_dest))

# For each plane, count the number of flights before the first delay of greater than 1 hour.
# confirmed that flight numbers are not reused within a given day -- i.e., do not need dep_time
flights %>%
    filter(!is.na(dep_delay)) %>%
    select(tailnum, year, month, day, dep_delay) %>%
    arrange(tailnum, year, month, day) %>%
    group_by(tailnum) %>%
    mutate(cumulative_hr_delays = cumsum(dep_delay > 60)) %>%
    summarise(total_flights = sum(cumulative_hr_delays < 1)) %>%
    arrange(total_flights)
