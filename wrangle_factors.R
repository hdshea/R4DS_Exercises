#' ---
#' title: "R4DS Wrangle Section: Factors Chapter"
#' author: "H. David Shea"
#' date: "21 January 2021"
#' output: github_document
#' ---
#'
library(tidyverse)

#' 15.3.1 exercise
#' 
#' Explore the distribution of rincome (reported income). What makes the default 
#' bar chart hard to understand? How could you improve the plot?
gss_cat %>% 
    count(rincome)

#' Explore the distribution of rincome (reported income).
#' What makes the default bar chart hard to understand? 
#' How could you improve the plot?
ggplot(gss_cat, aes(rincome)) +
    geom_bar() +
    coord_flip() +
    scale_x_discrete(drop = FALSE)
#' horizontal bars are easier to read
#' 

#' What is the most common relig in this survey? 
gss_cat %>% 
    count(relig, sort = TRUE) %>%
    head(1)
#' Protestant 10846
#' 

#' What’s the most common partyid?
gss_cat %>% 
    count(partyid, sort = TRUE) %>%
    head(1)
#' Independent         4119
#' 

#' Which relig does denom (denomination) apply to? 
#' How can you find out with a table? 
#' How can you find out with a visualization?
#' 
gss_cat %>% 
    count(relig, denom)

gss_cat %>% 
    count(relig, denom) %>%  
    ggplot(mapping = aes(x = relig, y = denom)) +
    geom_tile(mapping = aes(fill = n)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#' 15.4 modifying factor order
#' 

#' factor reordering - general
relig_summary <- gss_cat %>%
    group_by(relig) %>%
    summarise(
        age = mean(age, na.rm = TRUE),
        tvhours = mean(tvhours, na.rm = TRUE),
        n = n()
    )
ggplot(relig_summary, aes(tvhours, relig)) + geom_point()
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
    geom_point()

#' cases where factors have an implicit order shouldn't be reordered, in general
rincome_summary <- gss_cat %>%
    group_by(rincome) %>%
    summarise(
        age = mean(age, na.rm = TRUE),
        tvhours = mean(tvhours, na.rm = TRUE),
        n = n()
    )
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()

#' factor reordering - specific: fct_relevel
ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
    geom_point()
#' however, potentially benefited from specific case reordering
#' 

#' ordering factors by largest values of another values - fct_reorder2
by_age <- gss_cat %>%
    filter(!is.na(age)) %>%
    count(age, marital) %>%
    group_by(age) %>%
    mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
    geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
    geom_line() +
    labs(colour = "marital")

gss_cat %>%
    mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
    ggplot(aes(marital)) +
    geom_bar()

#' 15.4.1 exercises
#' 
#' There are some suspiciously high numbers in tvhours. 
#' Is the mean a good summary?
#' 
summary(gss_cat$tvhours)
#' mean is pretty close to median because the large outliers are few in number
#' but there are a large number of NAs as well
#' 

#' For each factor in gss_cat identify whether the order of the levels is arbitrary or 
#' principled.
#' 
levels(gss_cat$marital)
#' somewhat - never separated from versions of married
levels(gss_cat$race)
#' no
levels(gss_cat$rincome)
#' yes
levels(gss_cat$relig)
#' maybe some, but nothing consistent 
levels(gss_cat$denom)
#' somewhat - some logical groups within protestant
#' 

#' Why did moving “Not applicable” to the front of the levels move it to the bottom of 
#' the plot?
levels(gss_cat$rincome)
levels(fct_relevel(gss_cat$rincome, "Not applicable"))
#' as all unlisted levels stay the same, "Not app" became the first.  Default boxplot 
#' puts highest ordinal numbered factors from top down
#' 

#' 15.5 modifying factor levels
#' 

#' partyid has some continuity in factor (e.g., right to middle to left)
levels(gss_cat$partyid)
 
#' Category descriptions coud be better - consistent and more descriptive
gss_cat %>%
    mutate(partyid = fct_recode(partyid,
                                "Republican, strong"    = "Strong republican",
                                "Republican, weak"      = "Not str republican",
                                "Independent, near rep" = "Ind,near rep",
                                "Independent, near dem" = "Ind,near dem",
                                "Democrat, weak"        = "Not str democrat",
                                "Democrat, strong"      = "Strong democrat"
    )) %>%
    count(partyid)

#' and put all non specific answers in one group: other
gss_cat %>%
    mutate(partyid = fct_recode(partyid,
                                "Republican, strong"    = "Strong republican",
                                "Republican, weak"      = "Not str republican",
                                "Independent, near rep" = "Ind,near rep",
                                "Independent, near dem" = "Ind,near dem",
                                "Democrat, weak"        = "Not str democrat",
                                "Democrat, strong"      = "Strong democrat",
                                "Other"                 = "No answer",
                                "Other"                 = "Don't know",
                                "Other"                 = "Other party"
    )) %>%
    ggplot(aes(partyid)) +
    geom_bar() +
    coord_flip()
#'

#' lumping groups together: fct_lump
r1 <- gss_cat %>%
    count(relig, sort = TRUE) %>%
    transmute(relig, n_pre_lump = n)

r2 <- gss_cat %>%
    mutate(relig = fct_lump(relig, n = 10)) %>%
    count(relig, sort = TRUE) %>%
    transmute(relig, n_post_lump = n)

r1 %>% left_join(r2, by = "relig")

#' 15.5.1 exercises
#' 
#' How have the proportions of people identifying as Democrat, Republican, and 
#' Independent changed over time?
#' 
gss_cat %>%
    mutate(
        partyid = 
            fct_collapse(partyid,
                         Other = c("No answer", "Don't know", "Other party"),
                         Republican = c("Strong republican", "Not str republican"),
                         Independent = c("Ind,near rep", "Independent", "Ind,near dem"),
                         Democrat = c("Not str democrat", "Strong democrat")
            )
    ) %>%
    count(year, partyid) %>%
    group_by(year) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ggplot(aes(x = year, y = pct, color = fct_reorder2(partyid, year, pct))) +
    geom_line() +
    labs(color = "Political Party", x = "Year", y = "% of Population")

#' How could you collapse rincome into a small set of categories?
gss_cat %>%
    mutate(
        rincome = 
            fct_collapse(rincome,
                         "Other" = c("No answer", "Don't know", "Refused", "Not applicable"),
                         "Less Than $5000" = c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999"),
                         "$5000 - $9999"   = c("$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999")
            )
    ) %>%
    ggplot( aes(rincome)) +
    geom_bar() +
    coord_flip() +
    scale_x_discrete(drop = FALSE)
