#' ---
#' title: "R4DS Program Section: Functions Chapter"
#' author: "H. David Shea"
#' date: "22 January 2021"
#' output: github_document
#' ---
#'
#+ r setup, include = FALSE
library(tidyverse)
library(lubridate)
#+

#' ### 19.2 exercises
#' 
#' In the second variant of rescale01(), infinite values are left unchanged. 
#' Rewrite rescale01() so that -Inf is mapped to 0, and Inf is mapped to 1.
rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE, finite = TRUE)
    x[x == -Inf] <- rng[1]
    x[x == Inf] <- rng[2]
    (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(-Inf, 1:10, Inf))
#+ r dis1, include = FALSE
rm(rescale01)
#+

#' Practice turning the following code snippets into functions. Think about what each 
#' function does. What would you call it? How many arguments does it need? Can you 
#' rewrite it to be more expressive or less duplicative?
#' 
#' mean(is.na(x))
#' 
#' x / sum(x, na.rm = TRUE)
#' 
#' sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
#' 
pct_na <- function(x) {
    mean(is.na(x))
}
pct_na(c(1:9,NA))
pct_na(c(1:4,NA))
#' only self documenting, really - doesn't shorten code
#' 

pct_tot <- function(x, na.rm = FALSE) {
    x / sum(x, na.rm = na.rm)
}
pct_tot(c(1:5))
pct_tot(c(1:4, NA))
pct_tot(c(1:4, NA), na.rm = TRUE)
#' self documenting, allows user to determine NA handling; maybe eliminates calls to sum, but I doubt it
#' 

relative_sd <- function(x, na.rm = FALSE) {
# relative standard deviation (aka coefficient of variation)
    sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
relative_sd(rnorm(10))
relative_sd(c(1:4, NA))
relative_sd(c(1:4, NA), na.rm = TRUE)

my_var <- function(x, na.rm = FALSE) {
    x.ln <- length(x) - sum(is.na(x))
    x.mn <- mean(x, na.rm = na.rm)
    sum((x - x.mn)^2, na.rm = na.rm) / (x.ln - 1)
}
var(c(1:3,NA))
my_var(c(1:3,NA))
var(c(1:3,NA), na.rm = TRUE)
my_var(c(1:3,NA), na.rm = TRUE)

my_skew <- function(x, na.rm = FALSE) {
    x.ln <- length(x) - sum(is.na(x))
    x.mn <- mean(x, na.rm = na.rm)
    x.vr <- my_var(x, na.rm = na.rm)
    (sum((x - x.mn)^3, na.rm = na.rm) / (x.ln - 2)) / (x.vr^(3 / 2))
}
my_skew(rnorm(100))
my_skew(c(1,1,1,2,2,3))

both_na <- function(x,y) {
    sum(is.na(x) & is.na(y))
}
both_na(c(1:3,NA), c(rep(NA,4)))

#+ r dis2, include = FALSE
rm(both_na,my_skew,my_var,pct_na,pct_tot,relative_sd)
#+

#' ### 19.3 exercises
#' 
#' Read the source code for each of the following three functions, puzzle out what
#' they do, and then brainstorm better names
is_prefix <- function(string, prefix) {
    substr(string, 1, nchar(prefix)) == prefix
}
is_prefix("hello word", "foo")
is_prefix("foobar", "foo")

remove_last <- function(x) {
    if(length(x) <= 1) return(NULL)
    x[-length(x)]
}
remove_last(NA)
remove_last(c(1))
remove_last(c(1:2))

fill_to_match <- function(x, y) {
    rep(y, length.out = length(x))
}
fill_to_match(c(NA,NA,NA), "cat")
fill_to_match(c("cat", "dog", "pig"), NA)
fill_to_match(c("cat", "dog", "pig"), c(1,2))

#+ r dis3, include = FALSE
rm(is_prefix,remove_last,fill_to_match)
#+

#' ### 19.3 exercises
#' 
#' Write a greeting function that says “good morning”, “good afternoon”, or 
#' “good evening”, depending on the time of day. (Hint: use a time argument 
#' that defaults to lubridate::now(). That will make it easier to test your 
#' function.)
greeting <- function(time = lubridate::now()) {
    day_part <- "day"
    if (lubridate::is.timepoint(time)) {
        hr <- lubridate::hour(time)
        if (hr < 12) {
            day_part <- "morning"
        } else if (hr < 18) {
            day_part <- "afternoon"
        } else {
                day_part <- "evening"
        }
    }
    
    str_c("good", day_part, sep = " ")
}
greeting(now()+hours(4))
greeting(5)
greeting()

#' Implement a fizzbuzz function. It takes a single number as input. If the number 
#' is divisible by three, it returns “fizz”. If it’s divisible by five it returns 
#' “buzz”. If its divisible by three and five, it returns “fizzbuzz”. Otherwise, 
#' it returns the number. Make sure you first write working code before you create 
#' the function.
fizzbuzz <- function(x = NA) {
    rv <- ""

    # validate input as single length numeric
    stopifnot(length(x) == 1)
    stopifnot(is.numeric(x))
    if(x %% 3 == 0) {
        rv <- str_c(rv, "fizz")
    }
    if(x %% 5 == 0) {
        rv <- str_c(rv, "buzz")
    }
    if(str_length(rv) == 0) {
        rv <- str_c(rv, x)
    }

    rv
}
fizzbuzz(3)
fizzbuzz(5)
fizzbuzz(c(300,12))
fizzbuzz(11)
fizzbuzz("cat")
fizzbuzz()

#' How could you use cut() to simplify this set of nested if-else statements?
temp_factor <- function(temp) {
    factors <- c("freezing", "cold", "cool", "warm", "hot")
    breaks <- c(-Inf,0,10,20,30,Inf)
    as.character(cut(temp, breaks, factors))
}
temp_factor(-11)
temp_factor(30)
temp_factor(31)
temp_factor(c(-11,12,50))

#+ r dis4, include = FALSE
rm(fizzbuzz,greeting)
#+
