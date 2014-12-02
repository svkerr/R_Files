## Twitter Analysis
## Sources used in this project are:
## Twitter Client for R: Jeff Gentry
## R and Data Mining (Zhao)

## Load relevant packages:
library("twitteR")
library("tm")
library("wordcloud")
library("Snowball")
library("RWeka")
library("rJava")
library("RWekajars")
library("ggplot2")

## Retrieve the first 300 tweets (or all tweets if fewer than 300) from the
## user timeline of @rdatammining
## NOTE: "bigdata" are also good hashtags to do userTimeline() on
rdm.tweets <- userTimeline("rdatamining", n=300)
n.docs <- length(rdm.tweets)
n.docs
rdm.tweets[1:10]
rdm.tweets[100:110]

## Convert Tweets to a dataframe
tweet.dfrm <- do.call("rbind", lapply(rdm.tweets, as.data.frame))

## Build a corpus, and specify the source to be character vectors
my.corpus <- Corpus(VectorSource(tweet.dfrm$text))

## The corpus needs a couple of transformations, 
## including changing letters to lower case, and removing punctuations, numbers, and stop words. 
## Convert to lower case and remove punctuation
my.corpus <- tm_map(my.corpus,tolower)
my.corpus <- tm_map(my.corpus, removePunctuation)

## Remove numbers and URLS
my.corpus <- tm_map(my.corpus, removeNumbers)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
my.corpus <- tm_map(my.corpus, removeURL)

## Remove stopwords
## add two extra stop words, "available" and "via"
my.stopwords <- c(stopwords('english'), "available", "via")
## remove ""r" and "big" from stopwords
my.stopwords <- setdiff(my.stopwords, c("r", "big"))
## Remove stopwords from corpus
my.corpus <- tm_map(my.corpus,removeWords,my.stopwords)

## Stemming Words
## Keep a copy of corpus to use later as a dictionary for stem completion
my.corpus.copy <- my.corpus
my.corpus <- tm_map(my.corpus,stemDocument)  # Getting java error
## Stem completion
my.corpus <- tm_map(my.corpus, stemCompletion, dictionary=my.corpus.copy)

## Building a Term Document Matrix
my.tdm <- TermDocumentMatrix(my.corpus, control=list(wordLengths=c(1,Inf)))
my.tdm
## list the Terms
rownames(my.tdm)
## NOTE: Based upon the above matrix, may data mining tasks can be done.
## For example: clustering, classification and association analysis

## Inspect frequent terms:
findFreqTerms(my.tdm, lowfreq=10)
term.frequency <- rowSums(as.matrix(my.tdm))
term.frequency <- subset(term.frequency, term.frequency >= 10)
qplot(names(term.frequency), term.frequency, geom="bar", xlab="Terms") + coord_flip()

## Find Associations:
findAssocs(my.tdm,'data', 0.20)

## Word Cloud
m <- as.matrix(my.tdm)
## Calculate freq of words and sort it descendingly by frequency
word.freq <- sort(rowSums(m), decreasing=TRUE)
set.seed(375)   # For reproducibility
gray.levels <- gray( (word.freq+10) / (max(word.freq)+10))
wordcloud(words=names(word.freq), freq=word.freq, min.freq=3, random.order=F, colors=gray.levels)
#######################################################################
## Twitter Trend Analysis
yesterdayTrends <- getTrends('daily', date=as.character(Sys.Date()-1))
length(yesterdayTrends)
head(yesterdayTrends)
tail(yesterdayTrends)
yesterdayTrends

## Twitter search
sea <- searchTwitter('#datamining', n=50)
sea[1:10]