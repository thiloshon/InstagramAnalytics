require(instaR)
require(RCurl)
require(dplyr)
require(plyr)
require(RJSONIO)
require(ggplot2)
require(anytime)
require(scales)

## Authentication
appClientID <- "278fdad3738e444e8833c69bac6c3d1f"
appClientSecret <- "f50c37ec7f974886a25bb3a44b4fc8ee"
appAuth <-
    instaOAuth(appClientID,
               appClientSecret,
               scope = c("public_content", "follower_list", "basic"))
accessToken <- appAuth$credentials$access_token
save(accessToken, file = "auth")
load("auth")

save(insta.data.raw, file = "Data")
write.csv(insta.data.raw, file = "data.csv")
t<- read.csv("InstaAnalysis/dataNew.csv")
t<-t[,2:16]

## Owner Information
ownerInfo <-
    paste("https://api.instagram.com/v1/users/self/?access_token=",
          accessToken,
          sep = "")
ownerData <- getURL(ownerInfo) %>% fromJSON()
ownerData

# My Recent Data
recent <-
    paste(
        "https://api.instagram.com/v1/users/self/media/recent/?access_token=",
        accessToken,
        sep = ""
    )
recentPosts <- getURL(recent)
recentData <- fromJSON(recentPosts, flatten = T)
recentData

# Users Recent Data
recentOfUsers <- paste(
    "https://api.instagram.com/v1/users/",
    "2000582268",
    "/media/recent/?access_token=",
    accessToken,
    sep = ""
)
recentOfUserPosts <- getURL(recentOfUsers)
recentOfUsersData <- fromJSON(recentOfUserPosts, flatten = T)
recentOfUsersData

# Users i follow
follows <-
    paste(
        "https://api.instagram.com/v1/users/self/follows?access_token=",
        accessToken,
        sep = ""
    )
followsList.raw <- getURL(follows)
followsList <- fromJSON(followsList.raw, flatten = T)
followsList

# Users data

ramzan <-"1269207155"
supul <- "352993922"
shimak <- "1476182263"
ruzaik <- "1503992110"
thanu <- "2000582268"

usersData <-
    paste(
        "https://api.instagram.com/v1/users/",
        shimak,
        "/?access_token=",
        accessToken,
        sep = ""
    )
usersData.raw <- getURL(usersData)
UsersDataList <- fromJSON(usersData.raw, flatten = T)
UsersDataList



# Creating empty dataframe
insta.data.raw <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(insta.data.raw) <-
    c(
        "userName",
        "createdTime",
        "caption",
        "userHasLiked",
        "likes",
        "tags",
        "filter",
        "commentsCount",
        "type",
        "location",
        "usersInPhoto"
    )

# Looped data gathering
for (i in 1:20) {
    userName <- recentData$data[[i]]$user[[4]]
    createdTime <- recentData$data[[i]]$created_time
    
    if (is.null(recentData$data[[i]]$caption)) {
        caption <- "null"
    } else{
        caption <- recentData$data[[i]]$caption$text
    }
    
    userHasLiked <- recentData$data[[i]]$user_has_liked
    likes <- recentData$data[[i]]$likes[[1]]
    tags <- toString(recentData$data[[i]]$tags)
    filter <- recentData$data[[i]]$filter
    commentsCount <- recentData$data[[i]]$comments[[1]]
    type <- recentData$data[[i]]$type
    
    if (is.null(recentData$data[[i]]$location)) {
        location <- "null"
    } else{
        location <- recentData$data[[i]]$location$name
    }
    
    usersInPhoto <- length(recentData$data[[i]]$users_in_photo)
    
    insta.data.raw <-
        rbind(
            insta.data.raw,
            data.frame(
                userName = userName,
                createdTime = anytime(as.numeric(createdTime)),
                caption = caption,
                userHasLiked = userHasLiked,
                likes = likes,
                tags = tags,
                filter = filter,
                commentsCount = commentsCount,
                type = type,
                location = location,
                usersInPhoto = usersInPhoto
            )
        )
}

# Transformation
insta.data.raw$commentsAbove6 <- insta.data.raw$commentsCount > 6

insta.data.raw$weekdays <- weekdays(insta.data.raw$createdTime)

insta.data.raw$usersAbove0 <- insta.data.raw$usersInPhoto > 0

load("InstaAnalysis/data.csv")
insta.data.raw<- read.csv("InstaAnalysis/data.csv")

# Plotting
qplot(y = instagramData$likes,
      x = instagramData$weekdays,
      color = instagramData$userName) + labs(title = "Likes vs Weekdays", x = "Weekdays", y = "Likes", colour = "Users") 

qplot(
    y = insta.data.raw$likes,
    x = anydate(insta.data.raw$createdTime),
    color = insta.data.raw$usersAbove0
) + labs(title = "Likes vs Friends Tagged", x = "Time", y = "Likes", colour = "Is anyone tagged in the image?") 

qplot(
    y = instagramData$likes,
    x = anydate(instagramData$createdTime),
    color = (instagramData$followers > 200)
) + labs(title = "Likes vs Followers Count", x = "Time", y = "Likes", colour = "Is Followers > 200") 

qplot(
    y = instagramData$likes,
    x = anydate(instagramData$createdTime),
    color = as.factor(instagramData$followers)
) + labs(title = "Likes vs Followers Variation", x = "Time", y = "Likes", colour = "No of Followers") + geom_smooth(se=F, size = 0.8)

qplot(
    y = insta.data.raw$likes,
    x = anydate(insta.data.raw$createdTime),
    color = insta.data.raw$userName,
    
)

qplot(
    y = insta.data.raw$likes,
    x = insta.data.raw$filter,
    color = insta.data.raw$userName
)

qplot(
    y = insta.data.raw$likes,
    x = anydate(insta.data.raw$createdTime),
    color = insta.data.raw$commentsCount
)

qplot(y = insta.data.raw$likes,
      x = insta.data.raw$userName,
      color = insta.data.raw$userName, geom = "boxplot", 
      fill = insta.data.raw$userName) + labs(title = "Likes vs Users", x = "Users", y = "Likes", colour = "Users")

ggplot(data = insta.data.raw) + geom_boxplot(
    aes(
        y = insta.data.raw$likes,
        x = insta.data.raw$userName,
        fill = insta.data.raw$userName,
        colour = insta.data.raw$userName
    )
) + labs(
    title = "Likes vs Users",
    x = "Users",
    y = "Likes",
    fill = "Users",
    colour = "Users"
)



#regression or ML

fit <- lm(insta.data.raw$likes ~ ., data = insta.data.raw)
fit

fit2 <- lm(likes ~ as.Date(insta.data.raw$dateNew) + commentsCount + usersInPhoto + weekdays, data = insta.data.raw)
fit2
    
qplot(y = insta.data.raw$likes, x = insta.data.raw$createdTime) +
    scale_x_date(
        date_breaks = "1 month",
        labels = date_format("%b-%Y"),
        limits = as.Date(c('2014-07-01', '2016-07-01'))
    ) + geom_smooth()

ggplot(data = insta.data.raw,
       aes(y = insta.data.raw$likes, x = insta.data.raw$dateNew)) +
    scale_x_date(
        date_breaks = "06 month",
        labels = date_format("%b-%Y"),
        limits = as.Date(c('2014-09-02', '2017-05-01'), "%Y-%m-%d")
    ) + geom_smooth() + geom_point() 


qplot(y = insta.data.raw$likes, x = insta.data.raw$commentsCount) + geom_smooth()

cor(insta.data.raw[,c(5,8,11)])

library()
write.csv(cleanedInstaData, "cleanedData.csv")

train <- createDataPartition(cleanedInstaData$likes, p=0.75, list = F) 
data.train <- cleanedInstaData[train,]
data.test <- cleanedInstaData[-train,]



# Temp
recentData <- recentOfUsersData
insta.data.raw <- insta.data.raw[, 1:11]


# Dump
h <- length(names(recentData$data[[i]]))
l <- length(recentData$data[[i]]$users_in_photo)
print(h)
print(l)
length(recentData$data[[1]]$users_in_photo)

recentData$data[[1]]$users_in_photo

likedInfo <-
    paste(
        "https://api.instagram.com/v1/users/self/media/liked?access_token=",
        accessToken,
        sep = ""
    )
likedData <- getURL(likedInfo) %>% fromJSON()
likedData


iris.rg <-
    randomForest(
        Sepal.Length ~ .,
        data = iris,
        importance = TRUE,
        proximity = TRUE
    )



sqrt(sum((iris.rg$predicted - iris$Sepal.Length) ^ 2) / nrow(iris))

