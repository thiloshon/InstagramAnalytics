#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(anytime)
library(scales)

shinyUI(navbarPage(
    "Instagram Analytics",
    tabPanel("Analysis", {
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
                        "Sona" = "sona93",
                        "Supul" = "supulm",
                        "Shimak" = "shimak96",
                        "Ramzan" = "ramzandieze"
                    ),
                    selected = c(
                        "thiloshon",
                        "ruzaikjunaid",
                        "sona93",
                        "supulm",
                        "shimak96",
                        "ramzandieze"
                    )
                ),
                width = 3
            ),
            
            
            # Show a plot of the generated distribution
            
            mainPanel(
                h2("Welcome to Instagram Analytics"),
                h5("by Thiloshon Nagarajah on May 10, 2017"),
                p(
                    "This is a simple Shiny Application. The data was gathered using Instagram API. The app shows few findings from the data gathered. The date and users can be tweaked as you wish in the control panel."
                ),
                h3("Regression Line with Loess for Likes vs Time"),
                p("First let's see how Likes vary with Time"),
                plotOutput("distPlot"),
                p(
                    "The likes increase as the time increases. This makes sense since with increasing time, we will have increasing followers, with increasing followers we can expect more likes. But because of the limations of the Instagram API, i could only get followers count at the current date. I have assumed this is the same followers count throughout the time peroid which clearly is not the case."
                ),
                h3("Likes per Users"),
                p("Now lets see which user has more likes."),
                plotOutput("likesPerUserPlot"),
                p(
                    "Sona has more likes comparatively and variance of her like-counts are very less too. The thickness of the boxplot is very less in her plot. But ruzaikjunaid and thiloshon has more varience in the likes count. Shimak has interesting variation. His 90% of the times the likes vary within 30 - 40 but rest 10% of times, it goes as high as 70 and as low as 26. If you untick all users but shimak and check his Like vs Time graph (first graph), you can see his likes are within 30 - 40 till April 2016. After that the posts gets likes as high as 70. So i assume he got more followers in this April peroid which made spike in likes of posts after that. "
                ),
                
                h3("Likes per users tagged"),
                p(
                    "Now lets see if tagging additional users in the photo increases likes. Its intuitive, tagging users might bring more likes as it will reach more users. Lets see if this holds."
                ),
                plotOutput("userTaggedPlot"),
                p(
                    "As you can see tagging users has an interesting pattern. at the top of points we see tagged images having more likes (Blue dots at the top). But there is a confusion with the few points below (Blue below). The points despite of having tagged has low likes. Lets see if we can find out the reason for these outliers."
                ),
                plotOutput("likeVsFollowersPlot"),
                
                p(
                    "Here the plot shows if the posts had above 200 followers. From this we can conclude the low likes despite of tags might be due to the fact those users had less followers."
                ),
                p("Now lets see if the followers count has effect on likes "),
                plotOutput("followersPlot"),
                p(
                    "From the plot we can see low followers count correlates with low likes. But it doesnt show increase in likes with increase in followers. Its all jumbled up. This is because the data gathered doesnt show variation of followers with time. The data set is very important for analysis. My dataset is not a proper one."
                ),
                h3("Likes per Days"),
                p("Now lets see which days brings in more likes."),
                plotOutput("weekdayPlot"),
                p(
                    "From the plot, we can see days with more likes changes with user. But in general we can see thursdays bringing in more likes. Reason for Thursdays having more likes still buffles me. :) "
                ),
                
                h3("Filters By Users"),
                p("Now lets see the filters used by the users."),
                plotOutput("filterPlot"),
                p(
                    "Most of the times they have not used any filters. If they are using then Claredon and Juno has been used more. But it looks like filters doesnt have much influenze in likes. Now if you are interested in finding how much likes your post will get, click the next tab at the top of the page (Prediction Tab). To take a look at the complete dataset I gathered, click third tab (Complete Data Tab)"
                )
            )
        )
    }),
    tabPanel("Prediction", {
        sidebarLayout(
            sidebarPanel(
                dateInput("date", "Posting Date", value = NULL),
                selectInput("gender", "Gender", list("Male", "Female")),
                numericInput("followers", "Number of Followers", value = 250),
                numericInput("usersTagged", "Number of users tagged in post", value = 0),
                selectInput(
                    "location",
                    "Is Location Specified in Post?",
                    list("True", "False")
                ),
                numericInput("comments", "Number of Comments in the post", value = 0),
                selectInput(
                    "tags",
                    "Have you used any hashtags in post?",
                    list("True", "False")
                ),
                selectInput(
                    "caption",
                    "Have you added any caption in post?",
                    list("True", "False")
                ),
                selectInput(
                    "filters",
                    "The filter you have used",
                    list(
                        "Normal",
                        "Clarendon",
                        "Juno",
                        "Amaro",
                        "Gingham",
                        "Hefe",
                        "Hudson",
                        "Lark",
                        "Lo-fi",
                        "Ludwig",
                        "Mayfair",
                        "Nashville",
                        "Perpetua",
                        "Reyes",
                        "Rise",
                        "Sierra",
                        "Skyline",
                        "Sutro"
                    )
                ),
                actionButton("okay", "Okay, Predict Now!", width = '100%')
                
            ), 
            mainPanel(
                p("The predicted value isnt accurate at the moment. This is mainly because the variates I have collected could only explain around 60% of the varience. Due to the restrictions in Instagram API, i couldnt gather large amout of data either. This is just to get a rough understanding of what could be done. Meanwhile I am working on improving the quality of the dataset."),
                p("The likes expected for your post is: "),
                textOutput("predictedValue")
            )
            
        )
    }),
    tabPanel("Design",
             includeMarkdown("RMarkdownFile.md")
             
             #uiOutput('markdown')
            
             ),
    tabPanel("Complete Data", h3("Complete Dataset"),
             tableOutput("data"))
))