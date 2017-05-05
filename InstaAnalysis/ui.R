#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Instagram Analytics"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
                "bins",
                "Date Range:",
                start = "2014-09-02",
                end   = "2017-05-01",
                startview = "year"
            ),
            checkboxGroupInput(
                "users",
                "Users:",
                c(
                    "Thiloshon" = "thiloshon",
                    "Ruzaik" = "ruzaikjunaid",
                    "Thanu" = "thanu_naga",
                    "Supul" = "supulm",
                    "Shimak" = "shimak96",
                    "Ramzan" = "ramzandieze"
                ),
                selected = c(
                    "thiloshon",
                    "ruzaikjunaid",
                    "thanu_naga",
                    "supulm",
                    "shimak96",
                    "ramzandieze"
                )
            )
        ),
        
        # Show a plot of the generated distribution
        
        mainPanel(
            h2("Welcome to Instagram Analytics"),
            p("This is a simple Shiny Application. The data was gathered using Instagram API. The app shows few findings from the data gathered. The date can be tweaked to the first plot whereas the Users can be tweaked for other two outputs. "),
            h3("Linear Regression Line"),
            plotOutput("distPlot"),
            h3("Filters By Users"),
            plotOutput("filterPlot"),
            h3("Entire Data"),
            tableOutput("data")
        )
    )
))
