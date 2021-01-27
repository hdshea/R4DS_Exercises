R4DS Program Section: Functions Chapter
================
H. David Shea
22 January 2021

### 19.2 exercises

In the second variant of rescale01(), infinite values are left
unchanged. Rewrite rescale01() so that -Inf is mapped to 0, and Inf is
mapped to 1.

``` r
rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE, finite = TRUE)
    x[x == -Inf] <- rng[1]
    x[x == Inf] <- rng[2]
    (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(-Inf, 1:10, Inf))
```

    ##  [1] 0.0000000 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556
    ##  [8] 0.6666667 0.7777778 0.8888889 1.0000000 1.0000000

Practice turning the following code snippets into functions. Think about
what each function does. What would you call it? How many arguments does
it need? Can you rewrite it to be more expressive or less duplicative?

mean(is.na(x))

x / sum(x, na.rm = TRUE)

sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

``` r
pct_na <- function(x) {
    mean(is.na(x))
}
pct_na(c(1:9,NA))
```

    ## [1] 0.1

``` r
pct_na(c(1:4,NA))
```

    ## [1] 0.2

only self documenting, really - doesn’t shorten code

``` r
pct_tot <- function(x, na.rm = FALSE) {
    x / sum(x, na.rm = na.rm)
}
pct_tot(c(1:5))
```

    ## [1] 0.06666667 0.13333333 0.20000000 0.26666667 0.33333333

``` r
pct_tot(c(1:4, NA))
```

    ## [1] NA NA NA NA NA

``` r
pct_tot(c(1:4, NA), na.rm = TRUE)
```

    ## [1] 0.1 0.2 0.3 0.4  NA

self documenting, allows user to determine NA handling; maybe eliminates
calls to sum, but I doubt it

``` r
relative_sd <- function(x, na.rm = FALSE) {
# relative standard deviation (aka coefficient of variation)
    sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
relative_sd(rnorm(10))
```

    ## [1] -3.354594

``` r
relative_sd(c(1:4, NA))
```

    ## [1] NA

``` r
relative_sd(c(1:4, NA), na.rm = TRUE)
```

    ## [1] 0.5163978

``` r
my_var <- function(x, na.rm = FALSE) {
    x.ln <- length(x) - sum(is.na(x))
    x.mn <- mean(x, na.rm = na.rm)
    sum((x - x.mn)^2, na.rm = na.rm) / (x.ln - 1)
}
var(c(1:3,NA))
```

    ## [1] NA

``` r
my_var(c(1:3,NA))
```

    ## [1] NA

``` r
var(c(1:3,NA), na.rm = TRUE)
```

    ## [1] 1

``` r
my_var(c(1:3,NA), na.rm = TRUE)
```

    ## [1] 1

``` r
my_skew <- function(x, na.rm = FALSE) {
    x.ln <- length(x) - sum(is.na(x))
    x.mn <- mean(x, na.rm = na.rm)
    x.vr <- my_var(x, na.rm = na.rm)
    (sum((x - x.mn)^3, na.rm = na.rm) / (x.ln - 2)) / (x.vr^(3 / 2))
}
my_skew(rnorm(100))
```

    ## [1] -0.3392472

``` r
my_skew(c(1,1,1,2,2,3))
```

    ## [1] 0.7144345

``` r
both_na <- function(x,y) {
    sum(is.na(x) & is.na(y))
}
both_na(c(1:3,NA), c(rep(NA,4)))
```

    ## [1] 1

### 19.3 exercises

Read the source code for each of the following three functions, puzzle
out what they do, and then brainstorm better names

``` r
is_prefix <- function(string, prefix) {
    substr(string, 1, nchar(prefix)) == prefix
}
is_prefix("hello word", "foo")
```

    ## [1] FALSE

``` r
is_prefix("foobar", "foo")
```

    ## [1] TRUE

``` r
remove_last <- function(x) {
    if(length(x) <= 1) return(NULL)
    x[-length(x)]
}
remove_last(NA)
```

    ## NULL

``` r
remove_last(c(1))
```

    ## NULL

``` r
remove_last(c(1:2))
```

    ## [1] 1

``` r
fill_to_match <- function(x, y) {
    rep(y, length.out = length(x))
}
fill_to_match(c(NA,NA,NA), "cat")
```

    ## [1] "cat" "cat" "cat"

``` r
fill_to_match(c("cat", "dog", "pig"), NA)
```

    ## [1] NA NA NA

``` r
fill_to_match(c("cat", "dog", "pig"), c(1,2))
```

    ## [1] 1 2 1

### 19.3 exercises

Write a greeting function that says “good morning”, “good afternoon”, or
“good evening”, depending on the time of day. (Hint: use a time argument
that defaults to lubridate::now(). That will make it easier to test your
function.)

``` r
greeting <- function(time = lubridate::now()) {
    day_part <- "day"
    if(lubridate::is.timepoint(time)) {
        hr <- lubridate::hour(time)
        if(hr < 12) {
            day_part <- "morning"
        } else {
            if(hr < 18) {
                day_part <- "afternoon"
            } else {
                day_part <- "evening"
            }
        }
    }
    
    str_c("good", day_part, sep = " ")
}
greeting(now()+hours(4))
```

    ## [1] "good evening"

``` r
greeting(5)
```

    ## [1] "good day"

``` r
greeting()
```

    ## [1] "good afternoon"
