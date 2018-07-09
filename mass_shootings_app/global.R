
# Fetching data from - Mother Jones - Mass Shootings Database, 198 --------

library(tidyverse)
library(googlesheets)
library(magrittr)
library(plotly)
library(conflicted)
library(ggthemes)
library(stringr); library(stringi)
library(tidytext)
library(tm)


filter <- dplyr::filter # resolves filter-function conflict
as.Date <- base::as.Date

# register googlesheet from "Mother Jones - Mass Shootings Database 1982 - 2018

gs_mass_shootings <- gs_url(x = "https://docs.google.com/spreadsheets/d/1b9o6uDO18sLxBqPwl_Gh9bnhW-ev_dABH83M5Vb5L8o/edit#gid=0")

## Get data out of registered gs object

mass_shootings <- gs_mass_shootings %>% gs_read(ws = "Sheet1")

# Create state and city variables from location

mass_shootings %<>% separate(col = location, into = c("city", "state"), sep = ", ", remove = FALSE)

# Create data variable from date - some dates have 3/3/xx other have 3/3/xxxx

mass_shootings %<>% mutate(date = str_replace_all(date, pattern = "([0-9]{1,2}\\/[0-9]{1,2})\\/([0-9]{2}$)", replacement = "\\1\\/20\\2"))

mass_shootings %<>% mutate(date = base::as.Date(date, format = "%m/%d/%Y"))

## Change variable names with spaces or other symbols to underscore

names(mass_shootings) %<>% str_remove_all(pattern = "-|\\(|\\)") %>% str_replace_all(pattern = "[:space:]", replacement = "_") %>% str_replace_all(pattern = "__", replacement = "_")


## Clean up character-type variables

mass_shootings %<>% mutate(prior_signs_of_mental_health_issues = str_replace_all(prior_signs_of_mental_health_issues, pattern = "-", replacement = "TBD"), 
                           mental_health_details =  str_replace_all(mental_health_details, pattern = "-", replacement = "TBD"),
                           weapons_obtained_legally = str_replace_all(weapons_obtained_legally, pattern = "-", replacement = "TBD"),
                           where_obtained = str_replace_all(where_obtained, pattern = "-", replacement = "TBD"),
                           weapon_details = str_replace_all(mass_shootings$weapon_details, pattern = "^-$", replacement = "Unknown"),
                           weapons_obtained_legally = str_replace_all(weapons_obtained_legally, pattern = 'Yes \\(\\".+\\"\\)|^\\nYes', replacement = "Yes"),
                           weapons_obtained_legally = str_replace_all(weapons_obtained_legally, pattern = 'Kelley+', replacement = "Passed federal background check"))


## Clean up gender variable

mass_shootings %<>% mutate(gender = as.factor(gender))

mass_shootings %<>% mutate(gender = fct_collapse(gender, Male = c("M", "Male")))

## Collapse race variable

mass_shootings %<>% mutate(race = as.factor(race)) %>% mutate(race = fct_collapse(race, White = c("White", "white"), Black = c("Black", "black"), Unclear = c("-", "unclear")))


## Add key variable for plotting

mass_shootings %<>% mutate(key = row.names(mass_shootings))

# Plotly map - options ----------------------------------------------------


g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = toRGB("#8C96B3"),
        subunitwidth = 1,
        countrywidth = 1,
        subunitcolor = toRGB("white"),
        countrycolor = toRGB("white"),
        showlakes = TRUE,
        lakecolor = toRGB("#4C567A")
)

m <- list(
        l = 0,
        r = 5,
        b = 5,
        t = 30,
        pad = 2
)



# Moving average - plot processing ----------------------------------------

library(lubridate)
library(tidyquant)

# Rolling mean
mass_rolling_mean <- mass_shootings %>% 
        tq_mutate(
                # tq_mutate args
                select     = total_victims,
                mutate_fun = rollapply, 
                # rollapply args
                width      = 6,
                align      = "right",
                FUN        = mean,
                # mean args
                na.rm      = TRUE,
                # tq_mutate args
                col_rename = "roll_mean"
        )



# Number boxes ------------------------------------------------------------

## Preprocessing for value boxes

summary <- base::summary

# Precent gender

percent_gender <- mass_shootings %>% group_by(gender) %>% summarise(count = n()) %>% 
        mutate(percent = (count/sum(count))*100)

# Precent race

mass_shootings %>% group_by(race) %>% summarise(count = n()) %>% mutate(percent = count/sum(count))



# Text processing ---------------------------------------------------------

## Regex pattern to detect names and potential middle names (M. or full name) - acounts for del, -, III, Mc, etc

name_pattern <- "(?<!the )[A-Z][a-z]+ [A-Z]?[']?[A-Z][a-z]+ [A-Z][a-z]+\\, |^[A-Z|a-z]+ ([A-Z]\\.? )?[A-Z][a-z]+\\, |^[A-Z][a-z]+ [A-Z][a-z]+ (del)? [A-Z][a-z]+\\,? |[A-Z|a-z]+ ([A-Z]\\.? )?[A-Z][a-z]+\\, |[A-Z|a-z]+ [A-Z][a-z]+ III\\, |[A-Z|a-z]+ Mc[A-Z][a-z]+\\, |[A-Z][a-z]+\\-[A-Z][a-z]+ [A-Z][a-z]+\\, |[A-Z][a-z]+ [A-Z][a-z]+ [A-Z][a-z]+\\,? " # perl=TRUE

names <- str_extract_all(mass_shootings$summary, pattern = name_pattern)

## Entry 61 has a weird character in it - manual entry Sulejman Talović grepl(mass_shootings$summary, pattern = "\U{0107}")

names[61] <- "Sulejman Talović"

mass_shootings$name <- names %>% map(1) %>% unlist()# sapply(test, function(x) x[1])

# Delete all white spaces and commas in names

mass_shootings %<>% mutate(name = str_remove_all(name, pattern = ", $"))



# Text analysis -----------------------------------------------------------

## Normalize and clean text

# Remove white space in the beginning and end

mass_shootings %<>% mutate(summary_clean = str_replace(summary, pattern = "(^[:space:]?)", replacement = ""))

mass_shootings %<>% mutate(summary_clean = str_replace(summary, pattern = "([:space:]?$)", replacement = ""))


mass_shootings %<>% mutate(summary_clean = gsub("\r?\n|\r", " ", summary_clean))  # get rid of line brakes


mass_shootings %<>% mutate(summary_clean = str_replace_all(summary_clean, pattern = "([:space:]+)", replacement = " "))


## Split into sentences


sentences <- mass_shootings %>% unnest_tokens(output = sentence, input = summary_clean, token = "sentences")

str(sentences)

mass_sentences <- sentences %>% group_by(year) %>% mutate(linenumber = row_number()) %>% ungroup


## Creating tidy text one row per word data frame

tidy_mass <- mass_sentences %>% unnest_tokens(output = word, input = sentence)


data("stop_words")

library(tm)

# Also get stopwords from tm package

new_stops <- bind_rows(data.frame(word = stopwords("en"), lexicon = c("custom")), stop_words)

tidy_mass <- tidy_mass %>% anti_join(new_stops)



# Psychological descriptions ----------------------------------------------


## Normalize and clean text

# Remove white space in the beginning and end

mass_shootings %<>% mutate(mental_health_clean = str_replace(mental_health_details, pattern = "(^[:space:]?)", replacement = ""))

mass_shootings %<>% mutate(mental_health_clean = str_replace(mental_health_clean, pattern = "([:space:]?$)", replacement = ""))


mass_shootings %<>% mutate(mental_health_clean = gsub("\r?\n|\r", " ", mental_health_clean))  # get rid of line brakes


mass_shootings %<>% mutate(mental_health_clean = str_replace_all(mental_health_clean, pattern = "([:space:]+)", replacement = " "))

## Split into sentences

sentences_health <- mass_shootings %>% unnest_tokens(output = sentence, input = mental_health_clean, token = "sentences")


mass_sentences_health <- sentences_health %>% group_by(year) %>% mutate(linenumber = row_number()) %>% ungroup


## Creating tidy text one row per word data frame

tidy_mass_health <- mass_sentences_health %>% unnest_tokens(output = word, input = sentence)


## Removing stop words

tidy_mass_health <- tidy_mass_health %>% anti_join(new_stops)


