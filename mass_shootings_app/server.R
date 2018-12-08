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
library(plotly)
library(formattable)
library(fontawesome)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  output$plotly1 <- renderPlotly({
          
          gen_select <- switch(input$gender_select,
                               Male = "Male",
                               Female = "Female",
                               Both = "Male & Female",
                               All = c("Male", "Female", "Male & Female"))
          
          coloring_size <- switch(input$coloring_size,
                               Fatalities = "fatalities",
                               Injured = "injured",
                               TotalVictims = "total_victims")
          
        gen_mass <- mass_shootings %>% filter(gender %in% gen_select) %>% 
                filter(year >= input$from_year, year <= input$to_year)
        
          
        plot_geo(gen_mass, sizes = c(1, 250)) %>%
                add_markers(
                        x = ~longitude, y = ~latitude, color = ~gen_mass[[coloring_size]], size = ~gen_mass[[coloring_size]], colors=c("#E68415", "#C94024"), hoverinfo = "text", key = ~key,
                        text = ~paste("<b>", case,";", "</b>", "Year:", year, "<br>", "Location:", location,";", "Name: " , name, ";", "Gender: ", gender, ";", "<br>" , "<b>", "Total victims: " , total_victims, ";", "</b>", "Fatalities: " , fatalities, ";", "Injured: " , injured),
                        symbol = I("circle")
                ) %>%
                colorbar(title = str_to_title(coloring_size)) %>% 
                plotly::layout( 
                               geo = g, margin = m, mapbox = list(
                                       zoom = 100))
        


    
  })
  

  
 output$brush <- renderFormattable({
         
          d <- event_data("plotly_selected")

          if (is.null(d)) {formattable(tibble(What_to_do = "Click and drag events (i.e., box-select/lasso) appear here (double-click to clear)"))}
          
          else mass_shootings %>% 
                  filter(key %in% d$key) %>% 
                  select(name, age, gender, weapons_obtained_legally, weapon_details) %>% 
                  formattable(list(age = color_tile("orange", "red"), weapons_obtained_legally = formatter("span", 
                                                                        style = x ~ formattable::style(color = case_when(grepl("Yes", x) ~ "green",
                                                                                                                         grepl("No", x) ~ "red",
                                                                                                                         grepl("TBD", x) ~ "orange",
                                                                                                                         TRUE ~ "blue")), 
                                                                        x ~ icontext(case_when(grepl("Yes", x) ~ "ok-sign", 
                                                                                               grepl("No", x) ~ "remove-sign",
                                                                                               grepl("TBD", x) ~ "question-sign",
                                                                                               TRUE ~ "question-sign"), 
                                                                                     case_when(grepl("Yes", x) ~ "Yes",
                                                                                        grepl("No", x) ~ "No",
                                                                                        TRUE ~ "?"))), 
                                   gender = formatter("span", style = x ~ formattable::style(color = case_when(grepl("Female", x) | grepl("F", x) ~ "pink",
                                                                                                               grepl("Male", x) ~ "blue",
                                                                                                                grepl("Male & Female", x) ~ "black")),
                                                      x ~ icontext(case_when(grepl("Female", x) | grepl("F", x) ~ "user",
                                                                             grepl("Male", x) ~ "user",
                                                                             grepl("Male & Female", x) ~ "both")))))

        })
  

 
  
  output$plot2 <- renderPlot({
          
          gen_select <- switch(input$gender_select,
                               Male = "Male",
                               Female = "Female",
                               Both = "Male & Female",
                               All = c("Male", "Female", "Male & Female"))
          
          gen_mass <- mass_rolling_mean %>% filter(gender %in% gen_select)
          
          
                  ggplot(gen_mass, aes(x = date, y = total_victims)) +
                  # Data
                  geom_point(alpha = 0.8, color = ifelse(gen_mass$total_victims < 200,"#E68415", "#C94024"), size = 2.5) +
                  geom_line(aes(y = roll_mean), color = palette_light()[[1]], size = 1.2, alpha = .85) +
                  scale_x_date(date_breaks = "2 years", date_labels =  "%Y") +
                  theme_tufte() +
                  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
                  labs(y = "Total victims", x = "Year") +
                  coord_cartesian(xlim = c(lubridate::date("1982-01-01"), lubridate::date("2019-01-01")), ylim = c(0, 600))
          
          
  })
  

  
  output$plot3 <- renderPlot({
          
          
          tidy_mass %>% count(word, sort = TRUE) %>% filter(n > 10) %>% mutate(word = reorder(word, n)) %>% 
                  ggplot(aes(word, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() + 
                  scale_fill_gradient(guide = FALSE, low = "#E68415", high = "#C94024") +
                  theme_tufte() +
                  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
                  labs(y = "Count")
          

  })
  
  output$plot4 <- renderPlot({
          
          tidy_mass_health %>% count(word, sort = TRUE) %>% filter(n > 5) %>% mutate(word = reorder(word, n)) %>% 
                  ggplot(aes(word, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() +
                  scale_fill_gradient(guide = FALSE, low = "#E68415", high = "#C94024") +
                  theme_tufte() +
                  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
                  labs(y = "Count")
          
  })
  
})
