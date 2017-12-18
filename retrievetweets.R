# load relevant libraries
# set twitter api info

library(tm)
library(twitteR)
library(devtools)
library(streamR)
library(dplyr)
library(SnowballC)
library(rjson)
library(ROAuth)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "77wuXUI2sgQdoQEURpjZihtRX"
consumerSecret <- "azCnoTMP4mnzvkEvwGDqCfHFKSVo7NiGMWescXujceDrcN0vdU"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file="my_oauth")

load("my_oauth")

# GET TWEETS THAT MENTION RiHANNA

filterStream(file.name = "rihanna_tweets.json", # Save tweets in a json file
             track = c("@rihanna", "rihanna", "#rihanna"), #tracking handle, mentions, and hashtags
             timeout = 1000, # Keep connection alive for 1000 seconds
             oauth = my_oauth) # Use my_oauth file as the OAuth credentials

# GET TWEETS THAT MENTION TAYLOR SWIFT

filterStream(file.name = "taylorswift_tweets.json",
             track = c("@taylorswift13", "taylor swift", "#taylorswift"), 
             timeout = 1000,
             oauth = my_oauth)

# GET TWEETS THAT MENTION justin bieber

filterStream(file.name = "justinbieber_tweets.json",
             track = c("@justinbieber", "justin bieber", "#justinbieber"), 
             timeout = 1000, 
             oauth = my_oauth)

# GET TWEETS THAT MENTION KENDRICK LAMAR

filterStream(file.name = "kendricklamar_tweets.json",
             track = c("@kendricklamar", "kendrick lamar", "#kendricklamar"), 
             tweets = 2000, 
             oauth = my_oauth)

# GET TWEETS THAT MENTION RIHANNA IN THE US

filterStream(file.name = "rihanna_us_tweets.json",
             track = c("@rihanna", "rihanna", "#rihanna"), 
             location = c(-125,25,-66,50),
             timeout = 1000, 
             oauth = my_oauth)

# GET TWEETS THAT MENTION LADY GAGA IN THE US

filterStream(file.name = "taylorswift_us_tweets.json",
             track = c("@taylorswift13", "taylor swift", "#taylorswift"), 
             location = c(-125,25,-66,50),
             timeout = 1000, 
             oauth = my_oauth)

# GET TWEETS THAT MENTION justin bieber IN THE US

filterStream(file.name = "justinbieber_us_tweets.json",
             track = c("@justinbieber", "justin bieber", "#justintimberlake", "#justinbieber"), 
             location = c(-125,25,-66,50),
             timeout = 1000, 
             oauth = my_oauth)

# GET TWEETS THAT MENTION KENDRICK LAMAR IN THE US

filterStream(file.name = "kendricklamar_us_tweets.json",
             track = c("@kendricklamar", "kendrick lamar", "#kendricklamar"), 
             location = c(-125,25,-66,50),
             timeout = 1000, 
             oauth = my_oauth)

