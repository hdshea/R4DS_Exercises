#' ---
#' title: "R4DS Model Section: Model Building Chapter"
#' author: "H. David Shea"
#' date: "29 January 2021"
#' output: github_document
#' ---
#'
#+ r setup, include = FALSE
library(tidyverse)
library(modelr)
library(nycflights13)
library(lubridate)
library(splines)
options(na.action = na.warn)
#+

#' We’ll find patterns with visualisation, then make them concrete and precise 
#' with a model. We’ll then repeat the process, but replace the old response 
#' variable with the residuals from the model. The goal is to transition from 
#' implicit knowledge in the data and your head to explicit knowledge in a 
#' quantitative model. This makes it easier to apply to new domains, and easier 
#' for others to use.
#' 

#' ## 24.2 why are low quality diamonds more expensive?
#' 
#, cut:  Fair, Good, Very Good, Premium, Ideal
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
#' color:  D best, J worst
ggplot(diamonds, aes(color, price)) + geom_boxplot()
#' clarity:  I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best)
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

#' ### price & carat
#' 
#' There is an important confounding variable: the weight (carat) of the diamond. 
#' The weight of the diamond is the single most important factor for determining 
#' the price of the diamond, and lower quality diamonds tend to be larger.
ggplot(diamonds, aes(carat, price)) + 
    geom_hex(bins = 50)

diamonds %>% 
    mutate(vlarge = carat > 2.5) %>% 
    group_by(vlarge) %>% 
    count()
#' note that relationship is non-linear and most diamonds are <= 2.5 carat 
#' (above which the non-linear relationship tends to break down)
#' 
#' these transformations adjust for that
diamonds2 <- diamonds %>% 
    filter(carat <= 2.5) %>% 
    mutate(lprice = log2(price), lcarat = log2(carat))
ggplot(diamonds2, aes(lcarat, lprice)) + 
    geom_hex(bins = 50)

#' model the lprice ~ lcarat relationship
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>% 
    data_grid(carat = seq_range(carat, 20)) %>% 
    mutate(lcarat = log2(carat)) %>% 
    add_predictions(mod_diamond, "lprice") %>% 
    mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
    geom_hex(bins = 50) + 
    geom_line(data = grid, colour = "red", size = 1)

#' look at residuals - strong linear relation in log space is removed
diamonds2 <- diamonds2 %>% 
    add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
    geom_ref_line(h = 0, colour = "black") +
    geom_hex(bins = 50)

#' original quality charts with model price residuals
#' 
#' cut:  Fair, Good, Very Good, Premium, Ideal
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
#' color:  D best, J worst
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
#' clarity:  I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best)
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

#' ### a more complicated model
#' 
#' add the quality components into a more complicated model
#' 
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

#' plot new predictors individually
#' 
#' cut:  Fair, Good, Very Good, Premium, Ideal
grid <- diamonds2 %>% 
    data_grid(cut, .model = mod_diamond2) %>% 
    add_predictions(mod_diamond2)
grid
ggplot(grid, aes(cut, pred)) + 
    geom_point()

#' color:  D best, J worst
grid <- diamonds2 %>% 
    data_grid(color, .model = mod_diamond2) %>% 
    add_predictions(mod_diamond2)
grid
ggplot(grid, aes(color, pred)) + 
    geom_point()

#' clarity:  I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best)
grid <- diamonds2 %>% 
    data_grid(clarity, .model = mod_diamond2) %>% 
    add_predictions(mod_diamond2)
grid
ggplot(grid, aes(clarity, pred)) + 
    geom_point()

#' look at residuals of more complicated model
diamonds2 <- diamonds2 %>% 
    add_residuals(mod_diamond2, "lresid2")
ggplot(diamonds2, aes(lcarat, lresid2)) + 
    geom_hex(bins = 50)

#' shows some clear outliers
#' 
#' lresid2 > 1 -> price is more than twice as high as expected
#' 
#' lresid2 < -1 -> price is less than half as high as expected
diamonds2 %>% 
    filter(abs(lresid2) > 1) %>% 
    add_predictions(mod_diamond2) %>% 
    mutate(pred = round(2 ^ pred)) %>% 
    select(price, pred, carat:table, x:z) %>% 
    arrange(price)

#' ### 24.2 Exercises
#' 
#' Extract the diamonds that have very high and very low residuals. Is there anything 
#' unusual about these diamonds? Are they particularly bad or good, or do you think 
#' these are pricing errors?
#' 
diamonds2 %>% 
    filter(lresid2 >= 1) %>% 
    add_predictions(mod_diamond2) %>% 
    mutate(pred = round(2 ^ pred)) %>% 
    select(price, pred, carat:table, x:z) %>% 
    group_by(cut, color, clarity) %>% 
    count()
#' maybe some tendency for Fair cut, better color (D,E,F) and clarity (VS,VVS) to be over priced versus predicted in the outliers
#' 

#' Does the final model, mod_diamond2, do a good job of predicting diamond prices? 
#' Would you trust it to tell you how much to spend if you were buying a diamond?
#' 
#' raw residual frequency plot
ggplot(diamonds2, aes(lresid2)) + 
    geom_freqpoly(binwidth = 0.1)
#' absolute residual frequency plot
ggplot(diamonds2, aes(abs(lresid2))) + 
    geom_freqpoly(binwidth = 0.1)
#' some outliers in tail, but very low error/residual rate for most
#' 

#' ## 24.3 what affects the number of daily flights?
#' 
daily <- flights %>% 
    mutate(date = make_date(year, month, day)) %>% 
    group_by(date) %>% 
    summarise(n = n())
ggplot(daily, aes(date, n)) + 
    geom_line()

#' day of week effect dominates number of flights (_ah, remember business travel?_)
#' 
daily <- daily %>% 
    mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) + 
    geom_boxplot()

#' model day of week effect
#' 
mod <- lm(n ~ wday, data = daily)
#' predictions
grid <- daily %>% 
    data_grid(wday) %>% 
    add_predictions(mod, "n")
ggplot(daily, aes(wday, n)) + 
    geom_boxplot() +
    geom_point(data = grid, colour = "red", size = 4)

#' residuals
daily <- daily %>% 
    add_residuals(mod)
daily %>% 
    ggplot(aes(date, resid)) + 
    geom_ref_line(h = 0) + 
    geom_line()

#' new plot shows some subtler internal (w/o day of week) patterns
#' 
ggplot(daily, aes(date, resid, colour = wday)) + 
    geom_ref_line(h = 0) + 
    geom_line()
#' saturdays are under predicted in summer months and over predict in fall months
#' 

daily %>% 
    filter(resid < -100)
#' holidays dominate large outliers
#' 

daily %>% 
    ggplot(aes(date, resid)) + 
    geom_ref_line(h = 0) + 
    geom_line(colour = "grey50") + 
    geom_smooth(se = FALSE, span = 0.20, method = 'loess', formula = 'y ~ x')
#' there is a notable trend through the year
#' 

#' accounting for the seasonal saturday effect
#' 
daily %>% 
    filter(wday == "Sat") %>% 
    ggplot(aes(date, n)) + 
    geom_point() + 
    geom_line() +
    scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

#' use _school terms_ to break up year
term <- function(date) {
    cut(date, 
        breaks = ymd(20130101, 20130605, 20130825, 20140101),
        labels = c("spring", "summer", "fall") 
    )
}
daily <- daily %>% 
    mutate(term = term(date)) 
daily %>% 
    filter(wday == "Sat") %>% 
    ggplot(aes(date, n, colour = term)) +
    geom_point(alpha = 1/3) + 
    geom_line() +
    scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")
daily %>% 
    ggplot(aes(wday, n, colour = term)) +
    geom_boxplot()

#' model with term and compare results
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)
daily %>% 
    gather_residuals(without_term = mod1, with_term = mod2) %>% 
    ggplot(aes(date, resid, colour = model)) +
    geom_line(alpha = 0.75)
#' helps some in summer, but not so much other times
#' 

#' look at predictions by term with raw data
grid <- daily %>% 
    data_grid(wday, term) %>% 
    add_predictions(mod2, "n")
ggplot(daily, aes(wday, n)) +
    geom_boxplot() + 
    geom_point(data = grid, colour = "red") + 
    facet_wrap(~ term)
#' model is finding the _mean_ effect, but a lot of big outliers, so the mean can tend to be far away from the typical value.
#' 
#' Robust linear models help with this
#' 
#' MASS:rlm()
#' 
mod3 <- MASS::rlm(n ~ wday * term, data = daily)
daily %>% 
    add_residuals(mod3, "resid") %>% 
    ggplot(aes(date, resid)) + 
    geom_hline(yintercept = 0, size = 2, colour = "white") + 
    geom_line()

daily %>% 
    filter(abs(resid) > 100)
#' outliers are all holidays/holiday weekends again
#' 

#' time of year: an alternative approach
#' 
#' instead of using explicit domain knowledge (school term break points) try extracting from data
#' 
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)
daily %>% 
    data_grid(wday, date = seq_range(date, n = 13)) %>% 
    add_predictions(mod) %>% 
    ggplot(aes(date, pred, colour = wday)) + 
    geom_line() +
    geom_point()
#' same saturday trend as in raw data - that's good
#' 

#' ### 24.3 Exercises
#' 
#' Create a new variable that splits the wday variable into terms, but only for 
#' Saturdays, i.e. it should have Thurs, Fri, but Sat-summer, Sat-spring, Sat-fall. 
#' How does this model compare with the model with every combination of wday and term?
daily <- daily %>% 
    mutate(wday_satterm = ifelse(wday=="Sat",str_c(wday,term,sep="-"),str_c(wday))) 
mod3 <- MASS::rlm(n ~ wday * term, data = daily)
mod4 <- MASS::rlm(n ~ wday_satterm, data = daily)

daily %>% 
    gather_residuals(mod3,mod4) %>% 
    ggplot(aes(date, resid)) + 
    geom_hline(yintercept = 0, size = 2, colour = "white") + 
    geom_line(aes(color = model))

#' Create a new wday variable that combines the day of week, term (for Saturdays), 
#' and public holidays. What do the residuals of that model look like?
#'
#' I am defining holiday as all dates with abs(resid) > 100
daily <- daily %>% 
    mutate(wday100 = ifelse(
        abs(resid) > 100, "holiday",
        ifelse(wday=="Sat",str_c(wday,term,sep="-"),str_c(wday))))
mod3 <- MASS::rlm(n ~ wday * term, data = daily)
mod4 <- MASS::rlm(n ~ wday100, data = daily)

daily %>% 
    gather_residuals(mod3,mod4) %>% 
    ggplot(aes(date, resid)) + 
    geom_hline(yintercept = 0, size = 2, colour = "white") + 
    geom_line(aes(color = model))

#' What happens if you fit a day of week effect that varies by month (i.e. n ~ wday * month)? 
#' Why is this not very helpful?
daily <- daily %>% 
    mutate(month = month(date, label = TRUE, abbr = TRUE))
mod3 <- MASS::rlm(n ~ wday * term, data = daily)
mod4 <- MASS::rlm(n ~ wday * month, data = daily)
daily %>% 
    gather_residuals(mod3,mod4) %>% 
    ggplot(aes(date, resid)) + 
    geom_hline(yintercept = 0, size = 2, colour = "white") + 
    geom_line(aes(color = model))
#' wday x month = 7 x 12 = 84 parameters - way overfit with 365 obs
#' 

#' What would you expect the model n ~ wday + ns(date, 5) to look like? 
#' Knowing what you know about the data, why would you expect it to be 
#' not particularly effective?
print(summary(lm(n ~ wday + ns(date, 5), data = daily)))
#' wday '+' ns(date, 5) doesn't account for interactions which are the variations through the year
#' 


