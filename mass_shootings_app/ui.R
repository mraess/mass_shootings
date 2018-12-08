
library(shinymaterial)
library(fontawesome)
library(formattable)


# Wrap shinymaterial apps in material_page
ui <- material_page(

        title = paste(min(mass_shootings$year) ,"- ", max(mass_shootings$year) ,"US Mass Shootings"),
        nav_bar_fixed = TRUE,
        nav_bar_color = "indigo darken-1", 
        # Place side-nav in the beginning of the UI
        material_side_nav(
                fixed = FALSE,
                # Place side-nav tabs within side-nav
                material_side_nav_tabs(
                        side_nav_tabs = c(
                                "Map" = "nav1",
                                "Descriptions" = "nav2"
                        ),
                        icons = c("my_location", "line_style")
                )
        ),
        # Define side-nav tab content for nav1
        material_side_nav_tab_content(
                shinyjs::useShinyjs(),
                side_nav_tab_id = "nav1",
                tags$br(),
                
                material_row(
                             material_column(width = 3,
                                             material_dropdown(
                                                     input_id = "gender_select",
                                                     label = tags$p(fa("male", fill = "steelblue"),fa("female", fill = "pink"), "Select Gender:"),
                                                     choices = c("All", "Both", "Male", "Female"),
                                                     selected = "All",
                                                     multiple = FALSE,
                                                     color = "#3949ab"
                                             )),
                             
                             material_column(width = 3,
                                             material_slider(input_id = "from_year", 
                                                             label = "From year: ", 
                                                             min_value = min(mass_shootings$year), 
                                                             max_value = max(mass_shootings$year) - 1, 
                                                             initial_value = min(mass_shootings$year), 
                                                             color = "red"
                                             )),
                             material_column(width = 3,
                                             material_slider(input_id = "to_year", 
                                                             label = "To year: ", 
                                                             min_value = min(mass_shootings$year) + 1, 
                                                             max_value = max(mass_shootings$year), 
                                                             initial_value = max(mass_shootings$year), 
                                                             color = "red"
                                             )),
                             material_column(width = 3,
                                             material_radio_button(input_id = "coloring_size", 
                                                             label = "Coloring and size: ", 
                                                             choices = c("Fatalities", "Injured", "TotalVictims"), 
                                                             color = "red"
                                             ))),

                material_row(
                                material_column(
                                        width = 12,
                                        material_card(
                                                title = "US Mass Shootings 1982 - 2018",
                                                plotlyOutput("plotly1"),
                                                depth = 2
                                        )
                                )
                        ),
                
                material_row(
                        material_column(
                                width = 12,
                                material_card(
                                        title = "More Details",
                                        formattableOutput("brush"),
                                        depth = 2
                                )
                        )),
                
                material_row(
                        material_column(
                                width = 12,
                                material_card(
                                        title = "Moving average of total victims",
                                        plotOutput("plot2"),
                                        depth = 2
                                )
                        )
                ),

                
                material_row(
                        material_column(
                                width = 6,
                                tags$a(
                                        target = "_blank",
                                        class = "btn blue",
                                        href = "https://github.com/mraess/mass_shootings",
                                        "APP CODE"
                                )
                        )
                        
                )

),
material_side_nav_tab_content(
        side_nav_tab_id = "nav2",
        tags$br(),
        
        
        material_row(
                material_column(
                        width = 12,
                        material_card(
                                title = "Most frequent words: Summary of incident",
                                plotOutput("plot3"),
                                depth = 2
                        )
                )
        ),
        
        material_row(
                material_column(
                        width = 12,
                        material_card(
                                title = "Most frequent words: Psychological description",
                                plotOutput("plot4"),
                                depth = 2
                        )
                )
        ),
        
        material_row(
                material_column(
                        width = 6,
                        tags$a(
                                target = "_blank",
                                class = "btn blue",
                                href = "https://github.com/mraess/mass_shootings",
                                "APP CODE"
                        )
                )
                
        )
        
)

)
