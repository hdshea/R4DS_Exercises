#' ---
#' title: "R4DS Model Section: Model Basics Chapter"
#' author: "H. David Shea"
#' date: "28 January 2021"
#' output: github_document
#' ---
#'
#+ r setup, include = FALSE
library(tidyverse)
library(modelr)
options(na.action = na.warn)
#+

#' ## 23.2 a simple model
ggplot(sim1, aes(x, y)) + 
    geom_point()

models <- tibble(
    a1 = runif(250, -20, 40),
    a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
    geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
    geom_point() 

model1 <- function(a, data) {
    a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
    diff <- data$y - model1(mod, data)
    sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
    measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
    mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

ggplot(sim1, aes(x, y)) + 
    geom_point(size = 2, color = "grey30") + 
    geom_abline(
        aes(intercept = a1, slope = a2, color = -dist), 
        data = filter(models, rank(dist) <= 10)
    )

filter(models, rank(dist) <= 10)

ggplot(models, aes(a1, a2)) +
    geom_point(data = filter(models, rank(dist) <= 10), size = 4, color = "red") +
    geom_point(aes(color = -dist))

grid <- expand.grid(
    a1 = seq(-5, 20, length = 25),
    a2 = seq(1, 3, length = 25)
    ) %>% 
    mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
    ggplot(aes(a1, a2)) +
    geom_point(data = filter(grid, rank(dist) <= 10), size = 4, color = "red") +
    geom_point(aes(color = -dist))

ggplot(sim1, aes(x, y)) + 
    geom_point(size = 2, color = "grey30") + 
    geom_abline(
        aes(intercept = a1, slope = a2, color = -dist), 
        data = filter(grid, rank(dist) <= 10)
    )

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) + 
    geom_point(size = 2, color = "grey30") + 
    geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

#' ### 23.2 Exercises
#' 
#' One downside of the linear model is that it is sensitive to unusual values because 
#' the distance incorporates a squared term. Fit a linear model to the simulated data 
#' below, and visualise the results. Rerun a few times to generate different simulated 
#' datasets. What do you notice about the model?
#' 
sim1a <- tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rt(length(x), df = 2)
)

sim1a_mod <- lm(y ~ x, data = sim1a)
coef(sim1a_mod)

ggplot(sim1a, aes(x, y)) + 
    geom_point(size = 2, color = "grey30") + 
    geom_abline(intercept = coef(sim1a_mod)[1], slope = coef(sim1a_mod)[2])

#' One way to make linear models more robust is to use a different distance 
#' measure. For example, instead of root-mean-squared distance, you could use 
#' mean-absolute distance:
#' 
measure_distance2 <- function(mod, data) {
    diff <- data$y - model1(mod, data)
    mean(abs(diff))
}

sim1a <- tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rt(length(x), df = 2)
)

sim1a_mod <- lm(y ~ x, data = sim1a)
coef(sim1a_mod)

best <- optim(c(0, 0), measure_distance2, data = sim1a)
best$par

ggplot(sim1a, aes(x, y)) + 
    geom_point(size = 2, color = "grey30") + 
    geom_abline(intercept = coef(sim1a_mod)[1], slope = coef(sim1a_mod)[2]) +
    geom_abline(intercept = best$par[1], slope = best$par[2])


#' ## 23.3 visualizing models
#' 
#' It’s also useful to see what the model doesn’t capture, the so-called residuals which 
#' are left after subtracting the predictions from the data. Residuals are powerful 
#' because they allow us to use models to remove striking patterns so we can study the 
#' subtler trends that remain.
#' 
grid <- sim1 %>% 
    data_grid(x) 
grid

sim1_mod <- lm(y ~ x, data = sim1)

grid <- grid %>% 
    add_predictions(sim1_mod) 
grid

ggplot(sim1, aes(x)) +
    geom_point(aes(y = y)) +
    geom_line(aes(y = pred), data = grid, color = "red", size = 1)

sim1 <- sim1 %>% 
    add_residuals(sim1_mod)
sim1

#' freqpoly of residuals from lm(y ~ x)
ggplot(sim1, aes(resid)) + 
    geom_freqpoly(binwidth = 0.5)

#' plot of residuals from lm(y ~x)
ggplot(sim1, aes(x, resid)) + 
    geom_ref_line(h = 0) +
    geom_point() 

#' ### 23.3 Exercises
#' 
#' Instead of using lm() to fit a straight line, you can use loess() to fit a smooth 
#' curve. Repeat the process of model fitting, grid generation, predictions, and 
#' visualisation on sim1 using loess() instead of lm(). How does the result compare 
#' to geom_smooth()?
#' 
grid_ls <- sim1 %>% 
    data_grid(x) 
grid_ls

sim1_mod_ls <- loess(y ~ x, data = sim1)

grid_ls <- grid_ls %>% 
    add_predictions(sim1_mod_ls) 
grid_ls

ggplot(sim1, aes(x)) +
    geom_point(aes(y = y)) +
    geom_smooth(aes(y = y), size = 3) +
    geom_line(aes(y = pred), data = grid, color = "red", size = 1) +
    geom_line(aes(y = pred), data = grid_ls, color = "green", size = 1)

#'Why might you want to look at a frequency polygon of absolute residuals? 
#'What are the pros and cons compared to looking at the raw residuals?
#'

#' freqpoly of residuals from lm(y ~ x)
ggplot(sim1) + 
    geom_freqpoly(aes(resid), binwidth = 0.5) + 
    geom_freqpoly(aes(abs(resid)), binwidth = 0.5, color = "red")

#' ## 23.4 formulas and model families
#' 
#' categorical variables
#' 
ggplot(sim2) + 
    geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
    data_grid(x) %>% 
    add_predictions(mod2)
grid

#' Effectively, a model with a categorical x will predict the mean value for each 
#' category. (Why? Because the mean minimises the root-mean-squared distance.) 

ggplot(sim2, aes(x)) + 
    geom_point(aes(y = y)) +
    geom_point(data = grid, aes(y = pred), color = "red", size = 4)

#' interactions (continuous and categorical)
#' 
ggplot(sim3, aes(x1, y)) + 
    geom_point(aes(color = x2))

#' note '+' in mod1 and '*' in mod2
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>% 
    data_grid(x1, x2) %>% 
    gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, color = x2)) + 
    geom_point() + 
    geom_line(data = grid, aes(y = pred)) + 
    facet_wrap(~ model)

sim3 <- sim3 %>% 
    gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, color = x2)) + 
    geom_point() + 
    facet_grid(model ~ x2)

#' There is little obvious pattern in the residuals for mod2. The residuals for mod1 show 
#' that the model has clearly missed some pattern in b, and less so, but still present is 
#' pattern in c, and d. 


#' interactions (two continuous)
#' 
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
    data_grid(
        x1 = seq_range(x1, 5), 
        x2 = seq_range(x2, 5) 
    ) %>% 
    gather_predictions(mod1, mod2)
grid

#' Next let’s try and visualise that model. We have two continuous predictors, so you 
#' can imagine the model like a 3d surface. We could display that using geom_tile():
ggplot(grid, aes(x1, x2)) + 
    geom_tile(aes(fill = pred)) + 
    facet_wrap(~ model)

#' That doesn’t suggest that the models are very different! But that’s partly an illusion: 
#' our eyes and brains are not very good at accurately comparing shades of color. 
#' Instead of looking at the surface from the top, we could look at it from either side, 
#' showing multiple slices:
ggplot(grid, aes(x1, pred, color = x2, group = x2)) + 
    geom_line() +
    facet_wrap(~ model)
ggplot(grid, aes(x2, pred, color = x1, group = x1)) + 
    geom_line() +
    facet_wrap(~ model)

#' residuals
sim4 <- sim4 %>% 
    gather_residuals(mod1, mod2)

ggplot(sim4, aes(x1, resid, color = x2)) + 
    geom_point() + 
    facet_grid(model ~ x2)

#' transformations
#' 
df <- tribble(
    ~y, ~x,
    1,  1,
    2,  2, 
    3,  3
)
library(splines)
model_matrix(df, y ~ ns(x, 2)) # ns is natural spline function, second arg is degrees of freedom

sim5 <- tibble(
    x = seq(0, 3.5 * pi, length = 50),
    y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
    geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
    data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
    gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) + 
    geom_point() +
    geom_line(data = grid, color = "red") +
    facet_wrap(~ model)

#' Notice that the extrapolation outside the range of the data is clearly bad. This is the 
#' downside to approximating a function with a polynomial. But this is a very real 
#' problem with every model: the model can never tell you if the behaviour is true when 
#' you start extrapolating outside the range of the data that you have seen. You must 
#' rely on _theory_ and **science**. (Emphasis added by _president_ Joe Biden.)
#' 

#' ### 23.4 Exercises
#' 
#' What happens if you repeat the analysis of sim2 using a model without an 
#' intercept. What happens to the model equation? What happens to the 
#' predictions?
#' 
ggplot(sim2) + 
    geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)
mod2_no_int <- lm(y ~ x - 1, data = sim2)

grid <- sim2 %>% 
    data_grid(x) %>% 
    gather_predictions(mod2,mod2_no_int)
grid

#' exactly the same
#' 

#' Use model_matrix() to explore the equations generated for the models I fit to 
#' sim3 and sim4. Why is * a good shorthand for interaction?
#' 
model_matrix(y ~ x1 * x2, data = sim3)
model_matrix(y ~ x1 * x2, data = sim4)

#' For sim4, which of mod1 and mod2 is better? I think mod2 does a slightly better 
#' job at removing patterns, but it’s pretty subtle. Can you come up with a plot 
#' to support my claim?
#' 
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
#' predictions
grid <- sim4 %>% 
    data_grid(
        x1 = seq_range(x1, 5), 
        x2 = seq_range(x2, 5) 
    ) %>% 
    gather_predictions(mod1, mod2)
grid
#' residuals
sim4 <- sim4 %>% 
    gather_residuals(mod1, mod2)
sim4

#' nothing really jumps out in residual plots
#' 
#' raw
ggplot(sim4, aes(x1, resid, color = x2)) + 
    geom_ref_line(h = 0) +
    geom_point() + 
    facet_grid(model ~ x2)

#' absolute
ggplot(sim4, aes(x1, abs(resid), color = x2)) + 
    geom_ref_line(h = 0) +
    geom_point() + 
    facet_grid(model ~ x2)

#' nothing really jumps out in frequency residual plots
#' 
#' raw
ggplot(sim4, aes(resid, color = model)) + 
    geom_freqpoly(binwidth = 0.5)

#' absolute
ggplot(sim4, aes(abs(resid), color = model)) + 
    geom_freqpoly(binwidth = 0.5)

#' very slight variation in residual standard deviations - a _tad_ more in the mod1 tails???
sim4 %>%
    group_by(model) %>%
    summarise(
        resid_mn = round(mean(resid),3),
        resid_sd = round(sd(resid),3)
    )
