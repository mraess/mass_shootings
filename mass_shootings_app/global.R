
# Fetching data from - Mother Jones - Mass Shootings Database, 198 --------

library(tidyverse)
library(googlesheets)
library(magrittr)
library(plotly)

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


## Plot options

g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = toRGB("#4F2F2F"),
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