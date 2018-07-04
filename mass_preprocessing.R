library(tidyverse)

library(googlesheets)
library(magrittr)

# register googlesheet from "Mother Jones - Mass Shootings Database 1982 - 2018

gs_mass_shootings <- gs_url(x = "https://docs.google.com/spreadsheets/d/1b9o6uDO18sLxBqPwl_Gh9bnhW-ev_dABH83M5Vb5L8o/edit#gid=0")

gs_ws_ls(gs_mass_shootings)

## Get data out of registered gs object

mass_shootings <- gs_mass_shootings %>%
        gs_read(ws = "Sheet1")

## Data pre-processing

glimpse(mass_shootings)

# Create state and city variables from location

mass_shootings %<>% separate(col = location, into = c("city", "state"), sep = ", ", remove = FALSE)

# Create data variable from date - some dates have 3/3/xx other have 3/3/xxxx

mass_shootings %<>% mutate(date = str_replace_all(date, pattern = "([0-9]{1,2}\\/[0-9]{1,2})\\/([0-9]{2}$)", replacement = "\\1\\/20\\2"))

mass_shootings %<>% mutate(date = base::as.Date(date, format = "%m/%d/%Y"))

## Change variable names with spaces or other symbols to underscore

names(mass_shootings) %<>% str_remove_all(pattern = "-|\\(|\\)") %>% str_replace_all(pattern = "[:space:]", replacement = "_") %>% str_replace_all(pattern = "__", replacement = "_")


summary(mass_shootings)

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

levels(mass_shootings$gender)

mass_shootings %<>% mutate(gender = fct_collapse(gender, Male = c("M", "Male")))

mass_shootings %<>% mutate(race = as.factor(race)) %>% mutate(race = fct_collapse(race, White = c("White", "white"), Black = c("Black", "black"), Unclear = c("-", "unclear")))

levels(mass_shootings$race)

## Preprocessing for value boxes

summary <- base::summary

# Precent gender

percent_gender <- mass_shootings %>% group_by(gender) %>% summarise(count = n()) %>% 
        mutate(percent = (count/sum(count))*100)

percent_gender %>% filter(gender == "Female") %>% .$percent

# Precent race

mass_shootings %>% group_by(race) %>% summarise(count = n()) %>% mutate(percent = count/sum(count))
