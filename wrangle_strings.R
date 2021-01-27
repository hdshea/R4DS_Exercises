#' ---
#' title: "R4DS Wrangle Section: Strings Chapter"
#' author: "H. David Shea"
#' date: "20 January 2021"
#' output: github_document
#' ---
#'
library(tidyverse)

#' 14.4 Tools
#' 

#' 14.4.1 exercises
#' 

#' Find all words that start or end with x.
words[str_detect(words, "(^r|r$)")]
words[str_detect(words, "^r") | str_detect(words, "r$")]

#' Find all words that start with a vowel and end with a consonant.
words[str_detect(words, "^[aeiou].*[^aeiou]$")]
words[str_detect(words, "^[aeiou]") & str_detect(words, "[^aeiou]$")]

#' Are there any words that contain at least one of each different vowel?
#'
#' the regexp as a single would be way too long 
words[str_detect(words, "a") & str_detect(words, "e") & str_detect(words, "i") & str_detect(words, "o") & str_detect(words, "u")]

#' What word has the highest number of vowels? 
words[str_count(words, "[aeiou]")==max(str_count(words, "[aeiou]"))]
#' maybe more readable version
n_vowels <- str_count(words, "[aeiou]")
words[which(n_vowels == max(n_vowels))]

#' What word has the highest proportion of vowels? (Hint: what is the denominator?)
words[(str_count(words, "[aeiou]")/str_length(words))==max(str_count(words, "[aeiou]")/str_length(words))]
#' maybe more readable version
pct_vowels <- str_count(words, "[aeiou]")/str_length(words)
words[which(pct_vowels == max(pct_vowels))]

#' 14.4.2 extract matches
#' 
#' In the previous example, you might have noticed that the regular expression 
#' matched “flickered”, which is not a colour. Modify the regex to fix the problem.
colors <- c("(R|r)ed", "(O|o)range", "(Y|y)yellow", "(G|g)reen", "(B|b)lue", "(P|p)urple")
colors <- str_c("(^|\\s)", colors)
colors <- str_c(colors, "(\\s|.|$)")
color_match <- str_c(colors, collapse = "|")
more <- sentences[str_count(sentences, color_match) > 1]
str_view_all(more, color_match)

#' The first word from each sentence.
str_extract(sentences, "^[A-Za-z]+") %>% head

#' All words ending in ing.
str_extract_all(sentences, "\\b[A-Za-z]+ing\\b", simplify = TRUE)

#' 14.4.3 grouped matches
#' 
#' Find all words that come after a “number” like “one”, “two”, “three” etc. 
#' Pull out both the number and the word.
num_word_phrases <- "((O|o)ne|(T|t)wo|(T|t)hree|(F|f)our|(F|f)ive|(S|s)ix|(S|s)even|(E|e)ight|(N|n)ine|(T|t)en) ([^\\w]+)"
num_word_phrases <- "\\b([Oo]ne|[Tt]wo|[Tt]hree|[Ff]our|[Ff]ive|[Ss]ix|[Ss]even|[Ee]ight|[Nn]ine|[Tt]en) (\\w+)"

tibble(sentence = sentences) %>% 
    tidyr::extract(
        sentence, 
        c("num_word", "tgt_word"), 
        num_word_phrases, 
        remove = FALSE
    ) %>%
    filter(!is.na(num_word), !is.na(tgt_word))

#' Find all contractions. Separate out the pieces before and after the apostrophe.
tibble(sentence = sentences) %>% 
    tidyr::extract(
        sentence, 
        c("pre_part", "post_part"), 
        "\\b([A-Za-z]+)'([A-Za-z]+)", 
        remove = FALSE
    ) %>%
    filter(!is.na(pre_part), !is.na(post_part))
