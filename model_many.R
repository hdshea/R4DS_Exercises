#' ---
#' title: "R4DS Model Section: Many Models Chapter"
#' author: "H. David Shea"
#' date: "29 January 2021"
#' output: github_document
#' ---
#'
#+ r setup, include = FALSE
library(tidyverse)
library(modelr)
library(gapminder)
library(broom)
options(na.action = na.warn)
#+

#' ## 25.2 gap minder data 
#' 
#' never lose this:  https://www.youtube.com/watch?v=jbkSRLYSojo
#' 
gapminder %>% 
    ggplot(aes(year, lifeExp, group = country)) +
    geom_line(alpha = 1/3)

# single country - New Zealand 
nz <- filter(gapminder, country == "New Zealand")
nz %>% 
    ggplot(aes(year, lifeExp)) + 
    geom_line() + 
    ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
    add_predictions(nz_mod) %>%
    ggplot(aes(year, pred)) + 
    geom_line() + 
    ggtitle("Linear trend + ")

nz %>% 
    add_residuals(nz_mod) %>% 
    ggplot(aes(year, resid)) + 
    geom_hline(yintercept = 0, colour = "white", size = 3) + 
    geom_line() + 
    ggtitle("Remaining pattern")

#' nested data
#' 
by_country <- gapminder %>% 
    group_by(country, continent) %>% 
    nest()
by_country
#' just Afghanistan
by_country[[1,3]]

#' list-columns
#' 
country_model <- function(df) {
    lm(lifeExp ~ year, data = df)
}
by_country <- by_country %>% 
    mutate(model = map(data, country_model))
by_country

by_country %>% 
    filter(continent == "Oceania")
by_country %>% 
    arrange(continent, country)

#' unnesting
#' 
#' add residuals for each model-data pair
by_country <- by_country %>% 
    mutate(
        resids = map2(data, model, add_residuals)
    )
by_country
#' just Afghanistan
by_country[[1,5]]

#' use unnest to extract back to regular data frame
#' 
resids <- unnest(by_country, resids)
resids

#' plot residuals grouped by country
resids %>% 
    ggplot(aes(year, resid)) +
    geom_line(aes(group = country), alpha = 0.33) + 
    geom_smooth(se = FALSE)

#' facet by continent
resids %>% 
    ggplot(aes(year, resid, group = country)) +
    geom_line(alpha = 1 / 3) + 
    facet_wrap(~continent)

#' model quality
#' 
#' broom::glance extracts model quality metrics
glance(nz_mod)

by_country %>% 
    mutate(glance = map(model, glance)) %>% 
    unnest(glance)

glance <- by_country %>% 
    mutate(glance = map(model, glance)) %>% 
    unnest(glance, .drop = TRUE)
glance

glance %>% 
    arrange(r.squared)

glance %>% 
    ggplot(aes(continent, r.squared)) + 
    geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)
gapminder %>% 
    semi_join(bad_fit, by = "country") %>% 
    ggplot(aes(year, lifeExp, colour = country)) +
    geom_line()
#' We see two main effects here: the tragedies of the HIV/AIDS epidemic and the Rwandan genocide.
#' 

#' ### 25.2 Exercises
#' 
#' A linear trend seems to be slightly too simple for the overall trend. Can you do 
#' better with a quadratic polynomial? How can you interpret the coefficients of the 
#' quadratic? (Hint you might want to transform year so that it has mean zero.)
#' 
rm(glance)
by_country <- gapminder %>% 
    group_by(country, continent) %>% 
    nest()
country_model_poly <- function(df) {
    lm(lifeExp ~ poly(year - mean(year), 2), data = df)
}
by_country <- by_country %>% 
    mutate(model = map(data, country_model_poly))
by_country <- by_country %>% 
    mutate(resids = map2(data, model, add_residuals))
resids <- unnest(by_country, resids)

#' plot residuals for poly model grouped by country
resids %>% 
    ggplot(aes(year, resid)) +
    geom_line(aes(group = country), alpha = 0.33) + 
    geom_smooth(se = FALSE)

#' get poly model quality measures
by_country %>% 
    mutate(glance = map(model, glance)) %>% 
    unnest(glance)
glance <- by_country %>% 
    mutate(glance = map(model, glance)) %>% 
    unnest(glance, .drop = TRUE)
glance %>% 
    arrange(r.squared)
glance %>% 
    ggplot(aes(continent, r.squared)) + 
    geom_jitter(width = 0.5)
#' only Rwanda - *genocide impact* - remains distant outlier 
#' though there are other African countries and Cambodia that don't fit all that well (Rsq < 0.75)
#' 

#' Explore other methods for visualising the distribution of Rsq per continent. You might want to 
#' try the ggbeeswarm package, which provides similar methods for avoiding overlaps as jitter, but 
#' uses deterministic methods.
#' 
library("ggbeeswarm")
glance %>%
    ggplot(aes(continent, r.squared)) +
    geom_beeswarm()

#' To create the last plot (showing the data for the countries with the worst model fits), we 
#' needed two steps: we created a data frame with one row per country and then semi-joined it 
#' to the original dataset. It’s possible to avoid this join if we use unnest() instead of 
#' unnest(.drop = TRUE). How?
#' 
rm(glance)
by_country <- gapminder %>% 
    group_by(country, continent) %>% 
    nest() %>% 
    mutate(
        model = map(data, ~lm(lifeExp ~ poly(year - mean(year), 2), .)),
        glance = map(model, glance)
    ) %>% 
    unnest(glance) %>%
    unnest(data) %>% # if you don't drop data, the values can by unnested inline
    filter(r.squared < 0.75)
by_country %>% 
    ggplot(aes(year, lifeExp, color = country)) +
    geom_line()

#' ## 25.4 list-columns 

#' ### 25.4 Exercises
#' 
#' What’s missing in the following data frame? How does quantile() return that 
#' missing piece? Why isn’t that helpful here?
#' 
tmp <- mtcars %>%
    group_by(cyl) %>%
    summarise(q = list(quantile(mpg)))
tmp[[1,2]]
#' the specific quantile points are returned in the names of the results of quantile, but that
#' cannot be referenced within the piping framework
#' 

#' What does this code do? Why might might it be useful?
#' 
tmp <- mtcars %>% 
    group_by(cyl) %>% 
    summarise_all(list(list))
tmp[[1,2]]
tmp[[2,2]]
#' returns all observations of each variable in the dateset as a list for the grouping
#' might be useful for map function applications like with the linear modeling in the chapter
#' 


