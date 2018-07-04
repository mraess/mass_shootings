
library(shinymaterial)
library(shinymaterial)

# Wrap shinymaterial apps in material_page
ui <- material_page(
        title = "1982 - 2018 US Mass Shootings",
        nav_bar_fixed = TRUE,
        nav_bar_color = "indigo darken-1", 
        # Place side-nav in the beginning of the UI
        material_side_nav(
                fixed = TRUE,
                # Place side-nav tabs within side-nav
                material_side_nav_tabs(
                        side_nav_tabs = c(
                                "Map" = "nav1",
                                "Text" = "nav2"
                        ),
                        icons = c("list", "line_style")
                )
        ),
        # Define side-nav tab content
        material_side_nav_tab_content(
                side_nav_tab_id = "nav1",
                tags$br(),
                material_row(
                
                material_column(
                        material_card(
                                width = 20,
                                title = "US map with shootings",
                                plotlyOutput(outputId = "plotly1"),
                                depth = 2
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
                        
                ))
                ))
)
