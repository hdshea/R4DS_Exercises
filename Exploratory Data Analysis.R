### Exploratory Data Analysis

library(tidyverse)

# visualizing distributions

# categorical variables - bar charts
ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut))
diamonds %>% count(cut)

# continuous variables - histograms, freqpolys
ggplot(data = diamonds) +
    geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
diamonds %>% count(cut_width(carat, 0.5))

ggplot(data = diamonds, mapping = aes(x = carat, colour = cut)) +
    geom_freqpoly(binwidth = 0.5)

ggplot(data = diamonds, mapping = aes(x = carat)) +
    geom_histogram(binwidth = 0.01)

# inspecting unusual values

ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
    coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% 
    filter(y < 3 | y > 20) %>% 
    select(price, x, y, z) %>%
    arrange(y)
unusual

# 7.3 exercises

# Explore the distribution of each of the x, y, and z variables in diamonds. What 
# do you learn? Think about a diamond and how you might decide which dimension 
# is the length, width, and depth.

diamonds %>% 
    filter(x != 0, y != 0, z != 0, y < 30) %>%
    summarise(
        n = n(),
        x_avg = mean(x),
        x_std = sd(x),
        y_avg = mean(y),
        y_std = sd(y),
        z_avg = mean(z),
        z_std = sd(z)
    )
# x and y are likely length and width because they're equivalent, z then depth

diamonds %>% 
    filter(x != 0, y != 0, z != 0, x < 30, y < 30, z < 30) %>%
    ggplot(mapping = aes(x=x, y=y)) +
    geom_point()

diamonds %>% 
    filter(x != 0, y != 0, z != 0, x < 30, y < 30, z < 30) %>%
    ggplot(mapping = aes(x=x, y=z)) +
    geom_point()

diamonds %>% 
    filter(x != 0, y != 0, z != 0, x < 30, y < 30, z < 30) %>%
    ggplot(mapping = aes(x=y, y=z)) +
    geom_point()

# Explore the distribution of price. Do you discover anything unusual or surprising?
# (Hint: Carefully think about the binwidth and make sure you try a wide range of
# values.)
clean_diamonds <- diamonds %>%
    filter(
        !is.na(x), !is.na(y), !is.na(z), 
        x != 0, y != 0, z != 0, 
        x < 30, y < 30, z < 30
    )

ggplot(data = clean_diamonds, mapping = aes(x = price)) +
    geom_histogram(binwidth = 100)

clean_diamonds %>% count(cut_width(price, 100)) %>% filter(n < 100)
clean_diamonds %>% filter( price > 1450, price <= 1550)
# abnormally small number of diamonds in the $1450-$1550 price range
# bins on either side are near 1000 in count - there are only 66 in this bin

# How many diamonds are 0.99 carat? How many are 1 carat? What do you think is 
# the cause of the difference?
clean_diamonds %>% filter( carat == 0.99) %>% summarise( avg_price = mean(price))
clean_diamonds %>% filter( carat == 1.00) %>% summarise( avg_price = mean(price))
# 1.00 carat sells on average 19% more than 0.99 carat - perception premium

# Compare and contrast coord_cartesian() vs xlim() or ylim() when zooming 
# in on a histogram. What happens if you leave binwidth unset? What happens if 
# you try and zoom so only half a bar shows?
ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
    coord_cartesian(ylim = c(0, 50))

ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
    ylim(0, 50)

ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y)) +
    coord_cartesian(ylim = c(0, 50))

ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y)) +
    ylim(0, 50)

# Missing Values 

clean_diamonds <- diamonds %>% 
    mutate(
        x = ifelse(x < 3 | x > 20, NA, x),
        y = ifelse(y < 3 | y > 20, NA, y),
        z = ifelse(z < 3 | z > 20, NA, z)
    )

ggplot(data = clean_diamonds, mapping = aes(x = x, y = y)) + 
    geom_point()

# 7.4 exercises

# What happens to missing values in a histogram? What happens to missing values 
# in a bar chart? Why is there a difference?
ggplot(clean_diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(clean_diamonds) + 
    geom_bar(mapping = aes(x = y), width = 1)

# Covariation

## categorical vs continuous variable

ggplot(data = diamonds, mapping = aes(x = price)) + 
    geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(diamonds) + 
    geom_bar(mapping = aes(x = cut))

# density is the count standardized to each polygon area = 1
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
    geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
    geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
    geom_boxplot()

ggplot(data = mpg) +
    geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

ggplot(data = mpg) +
    geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
    coord_flip()

# 7.5.1 exercises

# Use what you’ve learned to improve the visualisation of the departure times of 
# cancelled vs. non-cancelled flights.

nycflt <- nycflights13::flights %>% 
    mutate(
        cancelled = is.na(dep_time),
        sched_hour = sched_dep_time %/% 100,
        sched_min = sched_dep_time %% 100,
        sched_dep_time = sched_hour + sched_min / 60
    )

ggplot(data = nycflt, mapping = aes(sched_dep_time)) + 
    geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

ggplot(data = nycflt, mapping = aes(x = sched_dep_time, y = ..density..)) + 
    geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

ggplot(data = nycflt, mapping = aes(x = cancelled, y = sched_dep_time)) +
    geom_boxplot()

# What variable in the diamonds dataset is most important for predicting the price 
# of a diamond? How is that variable correlated with cut? Why does the combination 
# of those two relationships lead to lower quality diamonds being more expensive?
# price ~ carat, cut, color, clarity

ggplot(data = clean_diamonds, mapping = aes(x = carat, y = price)) +
    geom_point()

ggplot(data = clean_diamonds, mapping = aes(x = cut, y = price)) +
    geom_boxplot()

ggplot(data = clean_diamonds, mapping = aes(x = color, y = price)) +
    geom_boxplot()

ggplot(data = clean_diamonds, mapping = aes(x = clarity, y = price)) +
    geom_boxplot()

ggplot(data = clean_diamonds, aes(x = cut, y = carat)) +
    geom_boxplot()
# some size / cut trade off in pricing

# Install the ggstance package, and create a horizontal boxplot. How does this 
# compare to using coord_flip()?

ggplot(data = clean_diamonds, aes(x = cut, y = carat)) +
    geom_boxplot() +
    coord_flip()

install.packages("ggstance")
library(ggstance)

ggplot(data = clean_diamonds, aes(x = cut, y = carat)) +
    geom_boxplot()

ggplot(data = clean_diamonds, aes(y = cut, x = carat)) +
    geom_boxplot()

# One problem with boxplots is that they were developed in an era of much smaller 
# datasets and tend to display a prohibitively large number of “outlying values”. One 
# approach to remedy this problem is the letter value plot. Install the lvplot package, 
# and try using geom_lv() to display the distribution of price vs cut. What do you 
# learn? How do you interpret the plots?

install.packages("lvplot")
library(lvplot)

ggplot(data = clean_diamonds, mapping = aes(x = cut, y = price)) +
    geom_boxplot()

ggplot(data = clean_diamonds, mapping = aes(x = cut, y = price)) +
    geom_lv()

# Compare and contrast geom_violin() with a facetted geom_histogram(), or a 
# coloured geom_freqpoly(). What are the pros and cons of each method?

ggplot(data = nycflt, mapping = aes(sched_dep_time)) + 
    geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

ggplot(data = nycflt, mapping = aes(sched_dep_time)) + 
    geom_histogram() +
    facet_wrap(~ cancelled, ncol = 1)

ggplot(data = nycflt, mapping = aes(y = sched_dep_time, x = cancelled)) + 
    geom_violin() +
    coord_flip()

# If you have a small dataset, it’s sometimes useful to use geom_jitter() to see 
# the relationship between a continuous and categorical variable. The ggbeeswarm 
# package provides a number of methods similar to geom_jitter(). List them and 
# briefly describe what each one does.

install.packages("ggbeeswarm")

## two categorical variables

ggplot(data = clean_diamonds) +
    geom_count(mapping = aes(x = cut, y = color))

clean_diamonds %>% 
    count(color, cut)

clean_diamonds %>% 
    count(color, cut) %>%  
    ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = n))

# 7.5.2 exercises

# Use proportions within groups
clean_diamonds %>% 
    count(color, cut) %>%  
    group_by(color) %>%
    mutate(pct = 100 * n / sum(n))

clean_diamonds %>% 
    count(color, cut) %>%  
    group_by(color) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = pct))

clean_diamonds %>% 
    count(color, cut) %>%  
    group_by(cut) %>%
    mutate(pct = 100 * n / sum(n))

clean_diamonds %>% 
    count(color, cut) %>%  
    group_by(cut) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = pct))

# Use geom_tile() together with dplyr to explore how average flight delays vary 
# by destination and month of year. What makes the plot difficult to read? How 
# could you improve it?

library(nycflights13)

flights %>%
    filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
    group_by(month,dest) %>%
    summarise(
        dep_delay_mean = mean(dep_delay),
        arr_delay_mean = mean(arr_delay)
    ) %>%
    ggplot(mapping = aes(x = factor(month), y = dest)) +
    geom_tile(mapping = aes(fill = arr_delay_mean))

## improvement ideas from jrnold.github.io
# remove small count airport - less than one flight per month
# reorder dest by avg delay
# add nice labels

flights %>%
    filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
    group_by(month,dest) %>%
    summarise(
        dep_delay_mean = mean(dep_delay),
        arr_delay_mean = mean(arr_delay)
    ) %>%
    group_by(dest) %>%
    filter(n() == 12) %>%
    ungroup() %>%
    mutate(dest = reorder(dest, dep_delay_mean)) %>%
    ggplot(mapping = aes(x = factor(month), y = dest)) +
    geom_tile(mapping = aes(fill = arr_delay_mean)) +
    labs(x = "Month", y = "Destination", fill = "Departure Delay")

# Why is it slightly better to use aes(x = color, y = cut) rather than 
# aes(x = cut, y = color) in the example above?

clean_diamonds %>% 
    count(color, cut) %>%  
    group_by(cut) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = pct))

clean_diamonds %>% 
    count(color, cut) %>%  
    group_by(cut) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ggplot(mapping = aes(x = cut, y = color)) +
    geom_tile(mapping = aes(fill = pct))


## two categorical variables

ggplot(data = clean_diamonds) +
    geom_point(mapping = aes(x = carat, y = price))

ggplot(data = clean_diamonds) + 
    geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

# 2-dimensional bin plots

install.packages("hexbin")

ggplot(data = clean_diamonds) +
    geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = clean_diamonds) +
    geom_hex(mapping = aes(x = carat, y = price))

# bin one dimension ~ categorical

ggplot(data = clean_diamonds, mapping = aes(x = carat, y = price)) + 
    geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))  # bins equal width

ggplot(data = clean_diamonds, mapping = aes(x = carat, y = price)) + 
    geom_boxplot(mapping = aes(group = cut_number(carat, 20)))  # bins equal number of obs

# 7.5.3 exercises

# Instead of summarising the conditional distribution with a boxplot, you could use a 
# frequency polygon. What do you need to consider when using cut_width() vs 
# cut_number()? How does that impact a visualisation of the 2d distribution of 
# carat and price?

ggplot(data = clean_diamonds, mapping = aes(color = cut_number(carat, 5), x = price)) +
    geom_freqpoly()

ggplot(data = clean_diamonds, mapping = aes(color = cut_width(carat, 1), x = price)) +
    geom_freqpoly()

# better width breaks
ggplot(data = clean_diamonds, mapping = aes(color = cut_width(carat, 1, boundary = 0), x = price)) +
    geom_freqpoly()

# Visualise the distribution of carat, partitioned by price.

ggplot(data = clean_diamonds, mapping = aes(x = price, y = carat)) + 
    geom_boxplot(mapping = aes(group = cut_width(price, 1000)))

ggplot(data = clean_diamonds, mapping = aes(x = price, y = carat)) + 
    geom_boxplot(mapping = aes(group = cut_width(price, 1000))) +
    coord_flip()

# How does the price distribution of very large diamonds compare to small 
# diamonds? Is it as you expect, or does it surprise you?
summary(clean_diamonds$carat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2000  0.4000  0.7000  0.7979  1.0400  5.0100 

clean_diamonds %>%
    mutate(size = ifelse(carat <= 1.0, "Smaller", "Larger")) %>%
    group_by(size) %>%
    summarise(
        price_avg = mean(price),
        price_std = sd(price)
    )

clean_diamonds %>%
    mutate(size = ifelse(carat <= 1.0, "Smaller", "Larger")) %>%
    ggplot(mapping = aes(x = size, y = price)) + 
    geom_boxplot()

clean_diamonds %>%
    mutate(size = ifelse(carat <= 1.0, "Smaller", "Larger")) %>%
    ggplot(mapping = aes(x = size, y = price)) + 
    geom_violin()

# Combine two of the techniques you’ve learned to visualise the combined 
# distribution of cut, carat, and price.

ggplot(data = clean_diamonds, mapping = aes(x = cut, y = price)) +
    geom_boxplot() +
    facet_wrap(~ cut_width(carat, 1, boundary = 0), ncol = 1)

# Two dimensional plots reveal outliers that are not visible in one dimensional plots. 
# For example, some points in the plot below have an unusual combination of x and 
# y values, which makes the points outliers even though their x and y values 
# appear normal when examined separately.

ggplot(data = clean_diamonds) +
    geom_point(mapping = aes(x = x, y = y)) +
    coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

clean_diamonds %>%
    filter(!near(x, y, tol = 0.4)) %>%
    ggplot() +
    geom_point(mapping = aes(x = x, y = y)) +
    coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

clean_diamonds %>%
    mutate(outlier = !near(x, y, tol = 0.4)) %>%
    ggplot() +
    geom_point(mapping = aes(x = x, y = y, color = outlier)) +
    coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

clean_diamonds %>%
    filter(!near(x, y, tol = 0.4))


## patterns and models

ggplot(data = faithful) + 
    geom_point(mapping = aes(x = eruptions, y = waiting))

# looking at residuals after modeling price ~ carat
library(modelr)

mod <- lm(log(price) ~ log(carat), data = clean_diamonds)

mod_diamonds <- clean_diamonds %>% 
    add_residuals(mod) %>% 
    mutate(resid = exp(resid))

ggplot(data = mod_diamonds) + 
    geom_point(mapping = aes(x = carat, y = resid))

# remove price ~ carat relationship (residuals) to see better cut residual price relationship
# relative to size (carat), better quality = higher price
ggplot(data = mod_diamonds) + 
    geom_boxplot(mapping = aes(x = cut, y = resid))

# quicker coding
ggplot(faithful, aes(eruptions)) + 
    geom_freqpoly(binwidth = 0.25)

clean_diamonds %>% 
    count(cut, clarity) %>% 
    ggplot(aes(clarity, cut, fill = n)) + 
    geom_tile()
