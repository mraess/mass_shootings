
library(shinymaterial)


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
                side_nav_tab_id = "nav1",
                tags$br(),
                

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
                                        title = "Moving average of total victims",
                                        plotOutput("plot2"),
                                        depth = 2
                                )
                        )
                ),
                material_row(
                        material_column(
                                width = 12,
                                material_card(
                                        title = "Table view",
                                        dataTableOutput("table1"),
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
