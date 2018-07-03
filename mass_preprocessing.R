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

library(plotly)


g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = toRGB("gray85"),
        subunitwidth = 1,
        countrywidth = 1,
        subunitcolor = toRGB("white"),
        countrycolor = toRGB("white")
)

plot_geo(mass_shootings, locationmode = 'USA-states', sizes = c(8, 250)) %>%
        add_markers(
                x = ~longitude, y = ~latitude, size = ~total_victims, color = ~gender, hoverinfo = "text",
                text = ~paste(mass_shootings$case, paste0("<b>","<i>",gender,"<i>", "      
                                                        </b>"), "Location:", location, total_victims, fatalities, injured)
        ) %>%
        layout(title = 'Mass shootings 1982 - 2018<br>(Click legend to toggle)', geo = g)


## Plotting with leaflet
library(leaflet)

## Make custom Twitter icons

# red = https://www.iconfinder.com/icons/738405/media_online_social_twitter_icon#size=128
# blue = https://www.iconfinder.com/icons/1233015/twitter_icon#size=128

twitterIconBlue <- makeIcon(
        iconUrl = "twitter_blue.png",
        iconWidth = 24,
        iconHeight = 24,
        iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

twitterIconRed <- makeIcon(
        iconUrl = "twitter_red.png",
        iconWidth = 24,
        iconHeight = 24,
        iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

## Adding popup-info to data frame

diss_part2 <- diss_part %>% mutate(gender = fct_recode(gender, "Female" = "female",
                                                       "Male" = "male")) %>% 
        mutate(popup_info = paste(sep = "<br/>", paste0("<b>","<i>",gender,"<i>", "      
                                                        </b>"), city, edu, edu2, tweet_num))

## Plotting the map

twitterIcons <- iconList(Male = twitterIconBlue, Female = twitterIconRed)



leaflet(mass_shootings) %>%
        addTiles() %>% 
        addMarkers(lng = ~longitude, 
                   lat = ~latitude,
                   popup = ~paste(mass_shootings$case), clusterOptions = markerClusterOptions())

# Alternative

leaflet(mass_shootings) %>%
        addProviderTiles(provider = providers$Esri.WorldStreetMap,
                         options = tileOptions(minZoom=2)) %>% 
        addMarkers(lng = ~longitude, 
                   lat = ~latitude,
                   popup = ~paste(mass_shootings$case, sep = "<br/>", paste0("<b>","<i>",gender,"<i>", "      
                                                        </b>"), "Location:", location, total_victims, fatalities, injured), clusterOptions = markerClusterOptions())


leaflet() %>% 
        addProviderTiles("Stamen.Watercolor")