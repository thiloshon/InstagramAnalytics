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
library(caret)
library(knitr)
library(stats)
library(randomForest)
set.seed(123)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$distPlot <- renderPlot({
        insta.data.raw <- read.csv("instagramData.csv")
        insta.data.raw <- insta.data.raw[, 2:15]
        insta.data.raw$dateNew <-
            as.Date(insta.data.raw$createdTime)
        dataNew <-
            insta.data.raw[insta.data.raw$userName %in% input$users,]
        
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
        insta.data.raw <- read.csv("instagramData.csv")
        dataNew <-
            insta.data.raw[insta.data.raw$userName %in% input$users,]
        ggplot(data = dataNew,
               aes(
                   y = dataNew$likes,
                   x = dataNew$filter,
                   color = dataNew$userName
               )) +
            geom_point() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(
                x = "Filters",
                y = "Likes",
                color = "Users",
                title = "Users and Filters Used"
            )
    })
    
    output$likesPerUserPlot <- renderPlot({
        instaData <- read.csv("instagramData.csv")
        dataNew <- instaData[instaData$userName %in% input$users,]
        
        ggplot(data = dataNew) + geom_boxplot(
            aes(
                y = dataNew$likes,
                x = dataNew$userName,
                fill = dataNew$userName,
                colour = dataNew$userName
            )
        ) + labs(
            title = "Likes vs Users",
            x = "Users",
            y = "Likes",
            fill = "Users",
            colour = "Users"
        )
    })
    
    output$weekdayPlot <- renderPlot({
        instaData <- read.csv("instagramData.csv")
        instagramData <-
            instaData[instaData$userName %in% input$users,]
        instagramData$weekdays <-
            factor(
                instagramData$weekdays,
                levels = c(
                    "Monday",
                    "Tuesday",
                    "Wednesday",
                    "Thursday",
                    "Friday",
                    "Saturday",
                    "Sunday"
                )
            )
        
        qplot(
            y = instagramData$likes,
            x = instagramData$weekdays,
            color = instagramData$userName
        ) + labs(
            title = "Likes vs Weekdays",
            x = "Weekdays",
            y = "Likes",
            colour = "Users"
        )
    })
    
    output$userTaggedPlot <- renderPlot({
        insta.data.raw <- read.csv("instagramData.csv")
        insta.data.raw$dateNew <-
            as.Date(insta.data.raw$createdTime)
        dataNew <-
            insta.data.raw[insta.data.raw$userName %in% input$users,]
        
        
        qplot(
            y = dataNew$likes,
            x = anydate(dataNew$createdTime),
            color = dataNew$usersAbove0
        ) + scale_x_date(
            date_breaks = "06 month",
            labels = date_format("%b-%Y"),
            limits = (input$bins)
        ) + labs(
            title = "Likes vs Friends Tagged",
            x = "Time",
            y = "Likes",
            colour = "Is anyone tagged in the image?"
        )
    })
    
    output$likeVsFollowersPlot <- renderPlot({
        insta.data.raw <- read.csv("instagramData.csv")
        insta.data.raw$dateNew <-
            as.Date(insta.data.raw$createdTime)
        instagramData <-
            insta.data.raw[insta.data.raw$userName %in% input$users,]
        
        qplot(
            y = instagramData$likes,
            x = anydate(instagramData$createdTime),
            color = (instagramData$followers < 200)
        ) + labs(
            title = "Likes vs Followers Count",
            x = "Time",
            y = "Likes",
            colour = "Is Followers < 200"
        )
    })
    
    output$followersPlot <- renderPlot({
        insta.data.raw <- read.csv("instagramData.csv")
        insta.data.raw$dateNew <-
            as.Date(insta.data.raw$createdTime)
        instagramData <-
            insta.data.raw[insta.data.raw$userName %in% input$users,]
        
        qplot(
            
            y = instagramData$likes,
            x = anydate(instagramData$createdTime),
            color = as.factor(instagramData$followers)
        ) + labs(
            title = "Likes vs Followers Variation",
            x = "Time",
            y = "Likes",
            colour = "No of Followers"
        ) + geom_smooth(se = F, size = 0.8)
    })
    
    
    cleanedInstaData <- read.csv("cleanedData.csv")
    cleanedInstaData <- cleanedInstaData[-c(1)]
    cleanedInstaData$dateNew <- as.POSIXct(cleanedInstaData$dateNew)
    
    train <- createDataPartition(cleanedInstaData$likes, p=0.75, list = F) 
    data.train <- cleanedInstaData[train,]
    data.test <- cleanedInstaData[-train,]
    load("rf.RData")
    #randomForest <- train(data.train[,-c(1)] , data.train$likes)
    
     #renderUI({
        #HTML(markdown::markdownToHTML(knit('RMarkdownFile.rmd', quiet = TRUE)))
   # })
    
    
    output$data <- renderTable({
        insta.data.raw <- read.csv("instagramData.csv")
        insta.data.raw[insta.data.raw$userName %in% input$users,-c(1, 2), drop = FALSE]
    }, rownames = TRUE)
    
     observeEvent( input$okay, {
        filter <- input$filters
        commentsCount <- input$comments
        usersInPhoto <- input$usersTagged
        dateNew <- as.POSIXct.Date( input$date)
        followers <- input$followers
        gender <- input$gender
        hasCaption <- input$caption
        hasTags <- input$tags
        hasLocation <- input$location
        
        newData <-
            data.frame(
                "filter" = filter,
                "commentsCount" = commentsCount,
                "usersInPhoto" = usersInPhoto,
                "dateNew" = dateNew,
                "followers" = followers,
                "gender" = gender,
                "hasCaption" = as.logical(hasCaption),
                "hasTags" = as.logical(hasTags),
                "hasLocation" = as.logical(hasLocation)
            )
        newData$filter <- factor(newData$filter, levels = levels(cleanedInstaData$filter))
        newData$gender <- factor(newData$gender, levels = c("Male", "Female"))
        
        predictions <- predict(randomForest, newdata=newData)
        output$newData <- renderText({
            newData
        }) 
        output$predictedValue <- renderText({
            predictions[1]
        }) 
        print(predictions)
        
    })
})
