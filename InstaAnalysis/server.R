#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(scales)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$distPlot <- renderPlot({
        insta.data.raw<- read.csv("dataNew.csv")
        insta.data.raw<-insta.data.raw[,2:15]
        insta.data.raw$dateNew <- as.Date(insta.data.raw$createdTime)
        print(insta.data.raw$dateNew)
        dataNew <-
            insta.data.raw[insta.data.raw$userName %in% input$users, ]
        
        ggplot(data = dataNew,
               aes(y = dataNew$likes, x = dataNew$dateNew)) +
            scale_x_date(
                date_breaks = "06 month",
                labels = date_format("%b-%Y"),
                limits = (input$bins)
            ) +
            geom_smooth() + xlab("Date") +
            ylab("Likes") + labs(title = "Likes over the Time", colour = "Users") +
            geom_point(aes(color = dataNew$userName))
        })
    
    #print(input$users)
    
    
    output$filterPlot <- renderPlot({
        insta.data.raw<- read.csv("dataNew.csv")
        dataNew <- insta.data.raw[insta.data.raw$userName %in% input$users, ]
        ggplot(data = dataNew,
               aes(
                   y = dataNew$likes,
                   x = dataNew$filter,
                   color = dataNew$userName
               )) + 
            geom_point() + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "Filters", y = "Likes", color = "Users", title = "Users and Filters Used")
    })
    
    
    output$data <- renderTable({
        insta.data.raw<- read.csv("dataNew.csv")
        insta.data.raw[insta.data.raw$userName %in% input$users, , drop = FALSE]
    }, rownames = TRUE)
    
})
