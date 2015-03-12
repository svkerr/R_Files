# Anomaly Detection using Twitter's anomaly detection library
# Ref: https://blog.twitter.com/2015/introducing-practical-and-robust-anomaly-detection-in-a-time-series
setwd("/Users/stuart/R_Files")

# Install packages
library(dplyr)
library(RJSONIO)
library(RCurl)
library(ggplot2)

install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)


# help for time series call of AnomalyDetection package
help(AnomalyDetectionTs)

# load test data
data(raw_data)
res <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
res$plot
attributes(res)
res$anoms

#From the plot, we can tell that the input time series experiences both positive and negative anomalies. Furthermore, many of the anomalies in the time series are local anomalies within the bounds of the time series’ seasonality.

#Therefore, these anomalies can’t be detected using the traditional methods. The anomalies detected using the proposed technique are annotated on the plot. In case the timestamps for the plot above were not available, anomaly detection could then be carried out using the AnomalyDetectionVec function. Specifically, you can use the following command:

res2 <- AnomalyDetectionVec(raw_data[,2], max_anoms=0.02, period=1440, direction='both', only_last=FALSE, plot=TRUE)
res2$plot

# Wikipedia analysis
page <- "Fairfax VA"
raw_data <- getURL(paste("http://stats.grok.se/json/en/latest90/", page, sep=""))
data <- fromJSON(raw_data)
views <- data.frame(timestamp=paste(names(data$daily_views), " 12:00:00", sep=""), stringsAsFactors=F)
views$count <- data$daily_views
views$timestamp <- as.POSIXlt(views$timestamp) # Transform to POSIX datetime
views <- views[order(views$timestamp),]
ggplot(views, aes(timestamp, count)) + geom_line() + scale_x_datetime() + xlab("") + ylab("views")
# Now, let’s look for anomalies. The usual way would be to feed a dataframe with a date-time and a value column into the AnomalyDetection function AnomalyDetectionTs(). But in this case, this doesn’t work because our data is much too coarse. It doesn’t seem to work with data on days. So, we use the more generic function AnomalyDetectionVec() that just needs the values and some definition of a period. In this case, the period is 7 (= 7 days for one week):
res3 <- AnomalyDetectionVec(views$count, max_anoms=0.05, direction='both', plot=TRUE, period=7)
res3$plot
res3$anoms
views[52,]

