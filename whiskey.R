# Set local working directory
setwd("/Users/stuart/Desktop/BigDataDocumentation/DataSets")

# Read in the data set
whiskies <- read.csv("whiskies.txt", row.names = 1, stringsAsFactors = FALSE)
str(whiskies)
sum(is.na(whiskies))   # check for missing observations
whiskies_k <- scale(whiskies[2:13])  # rescale selected vars for kmeans

ssPlot <- function(data,maxCluster = 9) {
  # Ititialize within sum of squares
  SSw <- (nrow(data) - 1) * sum(apply(data,2,var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:maxCluster,SSw,type = 'b', xlab = 'Number of Clusters',ylab = 'Within groups sum of squares')
}
ssPlot(whiskies_k)

#Naturally, the within groups sum of squares decreases as we increase the number of clusters. However, there is a trend of diminishing marginal returns as we increase the number of clusters. I select the number of clusters based on the point at which the marginal return of adding one more cluster is less than was the marginal return for adding the clusters prior to that.

# Here, i choose 4

fit <- kmeans(whiskies_k,4)
# append cluster assignment
whiskies <- data.frame(whiskies,fit$cluster)
whiskies$fit.cluster <- as.factor(whiskies$fit.cluster)

# Cluster centers can inform how taste profiles differ between clusters.
fit$centers

# Based on these centers, since i'm looking for a smoky taste, cluster 3 is my preferred whiskey cluster

subset(whiskies, fit.cluster == 3)

#The dataset contains coordinates that I used to investigate how flavor profiles differ geographically. The dataset's Latitude and Longitude variables are coordinates defined according to Great Britain's Ordnance Survey National Grid reference system. I converted the coordinates to standard latitude and longitude in order to plot them using ggmap.

library(maptools)
library(rgdal)
whiskies.coord <- data.frame(whiskies$Latitude, whiskies$Longitude)
coordinates(whiskies.coord) = ~whiskies.Latitude + whiskies.Longitude
head(whiskies.coord)
proj4string(whiskies.coord) = CRS("+init=epsg:27700") 
whiskies.coord <- spTransform(whiskies.coord, CRS("+init=epsg:4326"))
whiskies <- cbind(whiskies, whiskies.coord)
library(ggmap)


