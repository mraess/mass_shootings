#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymaterial)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(tm)
library(conflicted)
library(plotly)

filter <- dplyr::filter # resolves filter-function conflict

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$plotly1 <- renderPlotly({
    
          plot_geo(mass_shootings, sizes = c(1,250)) %>%
                  add_markers(
                          x = ~longitude, y = ~latitude, color = ~total_victims, size = ~total_victims, colors=c("#E68415", "#C94024"), hoverinfo = "text",
                          text = ~paste("<b>", mass_shootings$case, "</b>", "<br>", "Location:", location, "Gender: ", gender, "<br>" , "<b>", "Total victims: " , total_victims, "</b>", "Fatalities: " , fatalities, "Injured: " , injured),
                          symbol = I("circle")
                  ) %>%
                  colorbar(title = "Total victims") %>% 
                  plotly::layout(title = 'US Mass Shootings 1982 - 2018', 
                         geo = g, margin = m, mapbox = list(
                                 zoom = 100))

    
  })
  
})
