
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

plot_geo(mass_shootings, sizes = c(1,250)) %>%
        add_markers(
                x = ~longitude, y = ~latitude, color = ~total_victims, size = ~total_victims, colors=c("#E68415", "#C94024"), hoverinfo = "text",
                text = ~paste("<b>", mass_shootings$case, "</b>", "<br>", "Location:", location, "Gender: ", gender, "<br>" , "<b>", "Total victims: " , total_victims, "</b>", "Fatalities: " , fatalities, "Injured: " , injured),
                symbol = I("circle")
        ) %>%
        colorbar(title = "Total victims") %>% 
        layout(title = 'US Mass Shootings 1982 - 2018', 
               geo = g, margin = m, mapbox = list(
                       zoom = 100))
