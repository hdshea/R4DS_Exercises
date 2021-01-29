#' ---
#' title: "R4DS Program Section: Functions Chapter"
#' author: "H. David Shea"
#' date: "22 January 2021"
#' output: github_document
#' ---
#'
#+ r setup, include = FALSE
library(tidyverse)
#+

df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)
df
map_dbl(df, sd)

x1 <- list(
    c(0.27, 0.37, 0.57, 0.91, 0.20),
    c(0.90, 0.94, 0.66, 0.63, 0.06), 
    c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
    c(0.50, 0.72, 0.99, 0.38, 0.78), 
    c(0.93, 0.21, 0.65, 0.13, 0.27), 
    c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()

mtcars %>% 
    map_dbl(mean) %>% 
    round(2)

nycflights13::flights %>% 
    map_chr(typeof)

iris %>% 
    map(unique) %>% 
    map_int(length)

mns <- c(-10, 0, 10, 100)
mns %>% 
    map(rnorm, n = 10) %>% 
    str()



diamonds %>% 
    map_lgl(is.factor)

map(1:5, runif)

map(-2:2, rnorm, n = 5)
map_dbl(map(-2:2, rnorm, n = 100),mean)
map_dbl(map(-2:2, rnorm, n = 100),sd)

x <- split(mtcars,mtcars$cyl)
x %>% map(~lm(mpg ~ wt, data=.))


library(ggplot2)
plots <- mtcars %>% 
    split(.$cyl) %>% 
    map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())

head(iris %>% keep(is.factor))
head(iris %>% discard(is.factor))
head(iris)

my_every <- function(.v, .f) {
    rv <- TRUE
    for (i in seq_along(.v)) {
        rv <- rv & .f(.v[[i]])
    }
    rv
}
x <- c(0,1,0,1,0,1)
y <- as.logical(x)
my_every(x, is.logical)
my_every(y, is.logical)
every(x, is.logical)
every(y, is.logical)

rlang::is_true(TRUE)
rlang::is_true(1)

rlang::is_false(FALSE)
rlang::is_false(0)

col_sum3 <- function(df, f) {
    is_num <- sapply(df, is.numeric)
    df_num <- df[, is_num]
    
    sapply(df_num, f)
}
df <- tibble(
    x = 1:3, 
    y = 3:1,
    z = c("a", "b", "c")
)
# OK
col_sum3(df, mean)
# Has problems: don't always return numeric vector
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)

