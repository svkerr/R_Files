## Twitter Analysis
## Sources used in this project are:
## Twitter Client for R: Jeff Gentry
## R and Data Mining (Zhao)

# retrieve the first 200 tweets (or all tweets if fewer than 200) from the
# user timeline of @rdatammining
rdm.tweets <- userTimeline("rdatamining", n=200)
n.docs <- length(rdm.tweets)
rdm.tweets

## Convert Tweets to a dataframe
tweet.dfrm <- do.call("rbind", lapply(rdm.tweets, as.data.frame))

## Build a corpus (text repo). Ensure tm package is loaded. No longer a dataframe
my.corpus <- Corpus(VectorSource(tweet.dfrm$text))

## The corpus needs a couple of transformations, 
## including changing letters to lower case, and removing punctuations, numbers, and stop words. 
## Convert to lower case and remove punctuation
my.corpus <- tm_map(my.corpus,tolower)
my.corpus <- tm_map(my.corpus, removePunctuation)

## Remove numbers and URLS
myCorpus <- tm_map(myCorpus, removeNumbers)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)

## Twitter Trend Analysis
yesterdayTrends <- getTrends('daily', date=as.character(Sys.Date()-1))
length(yesterdayTrends)
head(yesterdayTrends)
tail(yesterdayTrends)
yesterdayTrends

## Twitter search
sea <- searchTwitter('#datamining', n=50)
sea[1:10]