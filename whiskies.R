# Set local working directory
setwd("/Users/stuart/Desktop/BigDataDocumentation/DataSets")

whiskies <- read.csv("whiskies.txt", row.names=1, stringsAsFactors = FALSE) 



sum(is.na(whiskies)) # no missing observations
whiskies_k <- scale(whiskies[2:13]) # rescale selected vars for kmeans



ssPlot <- function(data,maxCluster=9){
  # Initialize within sum of squares 
  SSw  <- (nrow(data)-1)*sum(apply(data,2,var))
  SSw <- vector()
  for (i in 2:maxCluster){
    SSw[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:maxCluster, SSw, type="b", 
       xlab="Number of Clusters",
       ylab="Within groups sum of squares") 
}
ssPlot(whiskies_k)


fit <- kmeans(whiskies_k, 4) # 4 cluster solution 

# append cluster assignment
whiskies <- data.frame(whiskies, fit$cluster)
whiskies$fit.cluster <- as.factor(whiskies$fit.cluster)



fit$centers 



subset(whiskies, fit.cluster==4)



whiskies_r <- whiskies[c(2:13,17)] 
# extract just flavor variables & cluster
candidates <- by (whiskies_r[-13], whiskies_r[13], function(data){
  # we apply this function to observations for each level of fit.cluster
  dists <- sapply(data, function(x) (x-mean(x))^2) 
  # for each variable, calc each observation's deviation 
  # from average of the variable across observations 
  dists <- rowSums(dists) 
  # for each observation, sum the deviations across variables
  rownames(data)[dists==min(dists)] 
  # obtain the row number of the smallest sum
})

candidates <- as.numeric(unlist(candidates))

whiskies[candidates,]



library(maptools)
library(rgdal)

whiskies.coord <- data.frame(whiskies$Latitude,whiskies$Longitude)
coordinates(whiskies.coord)=~whiskies.Latitude+whiskies.Longitude

proj4string(whiskies.coord)=CRS("+init=epsg:27700") # Specify that our coords are in osgb grid coord

whiskies.coord <- spTransform(whiskies.coord,CRS("+init=epsg:4326")) # spTransform to convert osgb grid to lat/lon

whiskies <- cbind(whiskies,whiskies.coord)



library("ggmap")


## whiskies <- cbind(whiskies, geocode(paste(whiskies$Location,"Scotland",sep=" ,")))




whiskyMap <- qmap(location = "Scotland", zoom = 6, maptype = "terrain", color = "bw", darken = .5)

# whiskyMap <- qmap(location = "Scotland", zoom = 6, legend = "topleft", maptype = "terrain", color = "bw", darken=.5)
# For some reason, the legend = "topleft" was causing the qmap function to crash

whiskyMap + geom_point(data=whiskies,
                       aes(x=whiskies.Latitude,
                           y=whiskies.Longitude,
                           colour=fit.cluster,
                           size=2))



whiskyMap <- qmap(location = "Islay", zoom = 10, legend = "topleft",
                  maptype = "terrain", color = "bw", darken=0.5)


whiskyMap + 
  geom_point()+
  geom_text(data=whiskies,
            aes(x=whiskies.Latitude,
                y=whiskies.Longitude,
                label=Distillery,
                color=fit.cluster,
                face="bold"))



## set.seed(1)


