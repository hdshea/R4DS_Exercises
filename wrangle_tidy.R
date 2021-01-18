### R4DS Wrangle Section
library(tidyverse)

# tidy case study on who dataset

## no non-unique instances of country, iso2, or iso3
who %>% select(country, iso2, iso3) %>% unique() %>%
    count(country) %>%
    filter(n>1)
who %>% select(country, iso2, iso3) %>% unique() %>%
    count(iso2) %>%
    filter(n>1)
who %>% select(country, iso2, iso3) %>% unique() %>%
    count(iso3) %>%
    filter(n>1)

# lengthen on likely values and remove NAs
tidy_who <- who %>% 
    pivot_longer(
        cols = new_sp_m014:newrel_f65, 
        names_to = "key", 
        values_to = "cases", 
        values_drop_na = TRUE
    )
tidy_who

# dataset contains TB case by country, year
# key identifies more breakdowns
# data dictionary for key: (new|old)_(type)_sexAgeGroup
# type = rel (relapse), ep (extrapulmonary), sn (smear negative), sp (smear positive)
# sex is f or m
# AgeGroup 014, 1524, 2534, 3544, 4554, 65

# review of key names shows a problem with newrel vs new_XX
# also might switch _f to _f_ and _m to _m_ to make separate work in one pass
view(count(tidy_who,key))

tidy_who <- tidy_who %>%
    mutate(key = str_replace(key, "newrel", "new_rel"),
           key = str_replace(key, "_f", "_f_"),
           key = str_replace(key, "_m", "_m_"))

tidy_who <- tidy_who %>%
    separate(key, c("new", "type", "sex", "agegroup"), sep = "_")

# new is redundant in this data set - remove it with extraneous country code values
tidy_who <- tidy_who %>%
    select(-new, -iso2, -iso3)

# all as one pipeline
tidy_who <- who %>% 
    pivot_longer(
        cols = new_sp_m014:newrel_f65, 
        names_to = "key", 
        values_to = "cases", 
        values_drop_na = TRUE
    ) %>%
    mutate(key = str_replace(key, "newrel", "new_rel"),
           key = str_replace(key, "_f", "_f_"),
           key = str_replace(key, "_m", "_m_")) %>%
    separate(key, c("new", "type", "sex", "agegroup"), sep = "_") %>%
    select(-new, -iso2, -iso3)

# For each country, year, and sex compute the total number of cases of TB. 
# Make an informative visualisation of the data.

tidy_who %>%
    group_by(country, year, sex) %>%
    summarise(
        cases = sum(cases)
    ) %>%
    filter(country == "Afghanistan") %>%
    ggplot() +
    geom_line(aes(year, cases)) +
    facet_wrap( ~ sex)

tidy_who %>%
    group_by(country, year, sex) %>%
    summarise(
        cases = sum(cases)
    ) %>%
    filter(country == "United States of America") %>%
    ggplot() +
    geom_line(aes(year, cases)) +
    facet_wrap( ~ sex)

view(tidy_who %>%
    group_by(country) %>% count())


