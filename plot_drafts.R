
# Plot_drafts -------------------------------------------------------------

library(plotly)


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

gen_mass <- mass_shootings %>% filter(gender == "Male")


plot_geo(gen_mass, sizes = c(1,250)) %>%
        add_markers(
                x = ~longitude, y = ~latitude, color = ~gen_mass$fatalities, size = ~gen_mass$fatalities, colors=c("#E68415", "#C94024"), hoverinfo = "text",
                text = ~paste("<b>", case,";", "</b>", "<br>", "Location:", location,";", "Name: " , name, ";", "Gender: ", gender, ";", "<br>" , "<b>", "Total victims: " , total_victims, ";", "</b>", "Fatalities: " , fatalities, ";", "Injured: " , injured),
                symbol = I("circle")
        ) %>%
        colorbar(title = str_to_title(gen_mass$fatalities)) %>% 
        plotly::layout(title = 'US Mass Shootings 1982 - 2018', 
               geo = g, margin = m, mapbox = list(
                       zoom = 100))

mass_shootings %>% gather(key = "kind", value = "count", fatalities, injured, total_victims) %>% 

ggplot(aes(x = year, y = count, fill = kind)) + geom_bar(stat = "identity", position = "dodge") + 
        theme_tufte()

mass_shootings$type_of_weapons

