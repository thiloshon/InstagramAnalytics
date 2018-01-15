---
title: "Design and Implementation"
author: "Thiloshon Nagarajah"
date: "5/10/2017"
output: html_document
---



## Data Retrieval

The user specific access token was used to get the instagram data. Various GET requests were used to retrieve as specified in the API. 


```r
appClientID <- "client ID goes here"
appClientSecret <- "client secret goes here"
appAuth <- instaOAuth(appClientID,
                      appClientSecret,
                      scope = c("public_content",
                                "follower_list",
                                "basic"))
accessToken <- appAuth$credentials$access_token

# JSON libraries were used to parse JSON data
ownerInfo <-
    paste("https://api.instagram.com/v1/users/self/?access_token=",
          accessToken,
          sep = "")
ownerData <- getURL(ownerInfo) %>% fromJSON()

# My Recent Data
recent <-
    paste(
        "https://api.instagram.com/v1/users/self/media/recent/?access_token=",
        accessToken,
        sep = ""
    )
recentPosts <- getURL(recent)
recentData <- fromJSON(recentPosts, flatten = T)

# Users Recent Data
recentOfUsers <- paste(
    "https://api.instagram.com/v1/users/",
    "2000582268",
    "/media/recent/?access_token=",
    accessToken,
    sep = ""
)

# Users i follow
follows <-
    paste(
        "https://api.instagram.com/v1/users/self/follows?access_token=",
        accessToken,
        sep = ""
    )
followsList.raw <- getURL(follows)
followsList <- fromJSON(followsList.raw, flatten = T)
```

## Feature Extraction

The raw data gathered was itself a feature extraction process. The features extracted were,



```r
        "userName", # the username of the user
        "createdTime", # time the post was created
        "caption", # caption of the post
        "userHasLiked", # has the user liked his own posts?
        "likes", # number of likes
        "tags", # the Hashtags used in the post
        "filter", # the filters used in the image
        "commentsCount", # number os counts
        "type", # is the post a video or image
        "location", # location of the post
        "usersInPhoto" # how many are tagged in the post?
        "followers" # how many users follow you?
```

## Introductory Analysis

The initial plots were plotted using ggplot. 


```r
qplot(y = instagramData$likes,
      x = instagramData$weekdays,
      color = instagramData$userName) + labs(
          title = "Likes vs Weekdays",
          x = "Weekdays",
          y = "Likes",
          colour = "Users"
      ) 

qplot(
    y = insta.data.raw$likes,
    x = anydate(insta.data.raw$createdTime),
    color = insta.data.raw$usersAbove0
) + labs(
    title = "Likes vs Friends Tagged",
    x = "Time",
    y = "Likes",
    colour = "Is anyone tagged in the image?"
) 

qplot(
    y = instagramData$likes,
    x = anydate(instagramData$createdTime),
    color = (instagramData$followers > 200)
) + labs(
    title = "Likes vs Followers Count",
    x = "Time",
    y = "Likes",
    colour = "Is Followers > 200"
)

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
```

## Training

The data was divided and trained.


```r
set.seed(123)
library(caret)
cleanedInstaData <- read.csv("cleanedData.csv")
cleanedInstaData <- cleanedInstaData[, -c(1)]
training <-
    createDataPartition(cleanedInstaData$likes, p = 0.75, list = F)
data.train <- cleanedInstaData[training, ]
data.test <- cleanedInstaData[-training, ]
```


```r
randomForest <- train(data.train[,-c(1)] , data.train$likes)
randomForest
```

