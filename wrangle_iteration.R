#' ---
#' title: "R4DS Program Section: Vectors Chapter"
#' author: "H. David Shea"
#' date: "25 January 2021"
#' output: github_document
#' ---
#'
#+ r setup, include = FALSE
library(tidyverse)
library(lubridate)
library(microbenchmark)
#+

rval <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
    rval[[i]] <- mean(mtcars[[i]], na.rm = TRUE)
}
rval

rval <- vector("character", ncol(nycflights13::flights))
for (i in seq_along(nycflights13::flights)) {
    rval[[i]] <- typeof(nycflights13::flights[[i]])
}
rval

rval <- vector("integer", ncol(iris))
for (i in seq_along(iris)) {
    rval[[i]] <- length(unique(iris[[i]]))
}
rval

rn.means <- c(-10, 0, 10, 100)
rval <- vector("list", length(rn.means))
for (i in seq_along(rn.means)) {
    rval[[i]] <- rnorm(10, mean = rn.means[i])
}
rval

out <- ""
for (x in letters) {
    out <- stringr::str_c(out, x)
}
out
#' shorter
str_c(letters, collapse = "")

x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
    sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
sd
#' shorter
sqrt(sum((x - mean(x)) ^ 2) / (length(x) - 1))


x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
    out[i] <- out[i - 1] + x[i]
}
out
#' shorter
out[2:length(x)] <- out[(2:length(x)) - 1] + x[2:length(x)]
out

#' Alice the Camel
humps <- c("five", "four", "three", "two", "one", "no")
for (x in humps) {
    for (y in 1:3) {
        cat(str_c("Alice the camel has", x, "humps.\n",sep = " "))
    }
    if (x != "no") {
        cat("So go, Alice, go.\n\n")
    } else {
        cat("Now Alice is a horse.\n\n")
    }
}

#' 99 bottle of beer on the wall
n_liq_surf <- function(bottles = 99, liquid = "beer", surface = "wall") {
    for (i in bottles:1) {
        what <- str_c(ifelse(i != 1," bottles"," bottle"), "of", liquid, sep=" ")
        where <- str_c(" on the", surface, sep=" ")

        cat(str_c(i, what, where, ".\n", sep=""))
        cat(str_c(i, what, ".\n", sep=""))
        cat("We take one down and pass it around.\n")
        what_left <- str_c(ifelse(i - 1 != 1," bottles"," bottle"), "of", liquid, sep=" ")
        cat(str_c(i - 1, what_left, where, ".\n\n", sep=""))
    }
}
n_liq_surf(5)
n_liq_surf(3, "whiskey", "floor")

#' nested versus pre space allocation
zed1 <- function(n) {
    output <- vector("integer", 0)
    for (i in seq_len(n)) {
        output <- c(output, i)
    }
    output
}

zed2 <- function(n) {
    output <- vector("integer", n)
    for (i in seq_len(n)) {
        output[[i]] <- i
    }
    output
}
microbenchmark(zed1(100), zed2(100), times = 50)

flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
    if (flip() == "H") {
        nheads <- nheads + 1
    } else {
        nheads <- 0
    }
    flips <- flips + 1
}
flips

files <- dir("./", pattern = "\\.R$", full.names = TRUE)
for (x in files) {
    cat(x, "\n")
}
for (nm in names(files)) {
    cat(nm, "\n")
}

names(files)[[1]] <- "First"
names(files)[[2]] <- "First"
names(files)[[length(files)]] <- "Last"
names(files)[[length(files)-1]] <- "Last"
for (nm in names(files)) {
    cat(nm, ":", files[nm], "\n")
}
names(files)
files

show_mean <- function(df) {
    for (nm in names(df)) {
        if (all(is.double(df[[nm]]))) {
            cat(nm, ":\t", str_c(round(mean(df[[nm]], na.rm = TRUE), 2)), "\n", sep="")
        }
    }
}
show_mean(iris)


#+ r dis1, include = FALSE
rm()
#+