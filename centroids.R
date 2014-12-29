# Hierarchial Clustering
library(flashClust)
pts = data.frame(x = c(1,1,2,3,2,3,4,4), y = c(1,2,4,4,5,5,1,2))
plot(pts,pch=16,cex=2,xlim=c(0,5), ylim=c(0,6))
axis(1,at=0:5)
axis(2,at=0:6)
d = dist(pts,method="euclidian",diag=TRUE,upper=TRUE)
round(d,2)

par(mfrow=c(1,2))
plot(flashClust(d,method="single"))
plot(flashClust(d,method="complete"))

d <- dist(pts, method="euclidian")
h <- flashClust(d, method="complete")
h$height


i <- which.max(diff(h$height))
cut_height <- (h$height[i] + h$height[i+1])/2
cut_height

clusters <- stats::cutree(h, h=cut_height)
clusters

par(mfrow=c(1,2))
plot(h)
abline(h=cut_height, col="red", lty=2)
plot(pts, col=c("red","green","blue")[clusters], pch=16)

# Read in Centroid Data #########################################
cent <-read.table("/Users/stuart/DataSets/centroids.txt", header=FALSE, sep=",", stringsAsFactors = FALSE)
x1 = cent$V2
y1 = cent$V1
centpts = data.frame(x1,y1)
d1 = dist(centpts, method="euclidian")
h1 = flashClust(d1,method="complete")
h1$height
i = which.max(diff(h1$height))
cut_height1 <- (h1$height[i] + h1$height[i+1])/2
cut_height1
# This heuristic results in an exceedingly high cut level
# Looking at a plot of the heights, eyeball knee in curve at about 
plot(h1$height)
abline(h = 30)
cut_height1 = 30
clusters1 <- stats::cutree(h1, h=cut_height1)
centpts_cluster = cbind(centpts,clusters1)
centpts_cluster
write.csv(centpts_cluster, file = "/Users/stuart/DataSets/centpts_cluster.csv",row.names=FALSE)
