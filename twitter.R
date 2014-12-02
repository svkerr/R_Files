## Authenticate with twitter API

library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
library(reshape2)

# install this file:
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

api_key <-"pZsNCkOSPb3oFlw6ZHf7q5rsc"
api_secret <-"bIEimC1fRBNf5b9sQyL0LboFsj2Y5sUVkXvmfEOQXJtiQmllOU"
access_token <-"75807850-pbK8CxU9DVN3T0XUbz0o56IdReNVqUs0ChpTIopU7"
access_token_secret <-"KxUU8YAzsUirBKW9QAh2Df1dpSZvAUb3bte05hziDZdSh"

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "http://api.twitter.com/oauth/authorize"
consumerKey = "pZsNCkOSPb3oFlw6ZHf7q5rsc"
consumerSecret = "bIEimC1fRBNf5b9sQyL0LboFsj2Y5sUVkXvmfEOQXJtiQmllOU"


Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret = consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL,
                         authURL=authURL)

Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package="RCurl"))
save(Cred, file="twitter_authentication.Rdata")
registerTwitterOAuth(Cred)

# For future use:
load("twitter_authentication.Rdata")

Athletics.list <- searchTwitter('#Athletics', n=1000, cainfo='cacert.pem')
Athletics.df <- twListToDF(Athletics.list)
str(Athletics.df)

# Sentiment Function


                     