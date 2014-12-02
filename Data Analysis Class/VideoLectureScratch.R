# Scratch area for Video Lectures
############################################################
Video Lecture: Week 4, Clustering Example
############################################################
file.url <- "https://dl.dropbox.com/u/7710864/courseraPublic/samsungData.rda" 
download.file(file.url, destfile="./data/samsungData.rda", method = "curl")
date.downloaded <- date()
load("./data/samsungData.rda")

## Explore the data
table(samsungData$activity)
table(samsungData$subject)
names(samsungData)

## Plotting Average Acceleration for first subject
numeric.activity <- as.numeric(as.factor(samsungData$activity))[samsungData$subject==1]
par(mfrow=c(2,2))
plot(samsungData[samsungData$subject==1,1],pch=19,col=numeric.activity,ylab=names(samsungData)[1])
plot(samsungData[samsungData$subject==1,2],pch=19,col=numeric.activity,ylab=names(samsungData)[2])
legend(150,-0.1,legend=unique(samsungData$activity),col=unique(numeric.activity),pch=19)
samsungData[samsungData$subject==1,1:3]  ## Plotting Average Accelerations for X,Y,Z of 1st subject

## Plotting Average Acceleration for third subject
numeric.activity3 <- as.numeric(as.factor(samsungData$activity))[samsungData$subject==3]
par(mfrow=c(1,2))
plot(samsungData[samsungData$subject==3,1],pch=19,col=numeric.activity3,ylab=names(samsungData)[1])
plot(samsungData[samsungData$subject==3,2],pch=19,col=numeric.activity3,ylab=names(samsungData)[2])
samsungData[samsungData$subject==3,1:3]

### Plotting Max Acceleration for first subject
par(mfrow=c(1,2))
plot(samsungData[samsungData$subject==1,10],pch=19,col=numeric.activity,ylab=names(samsungData)[10])
plot(samsungData[samsungData$subject==1,11],pch=19,col=numeric.activity,ylab=names(samsungData)[11])

### Singular Value Decomposition
svd1 <- svd(scale(samsungData[samsungData$subject==1,-c(562,563)]))
str(svd1)
par(mfrow=c(1,2))
plot(svd1$u[,1],col=numeric.activity)
plot(svd1$u[,2],col=numeric.activity)
legend(150,-0.1,legend=unique(samsungData$activity),col=unique(numeric.activity),pch=19)

par(mfrow=c(1,2))
plot(svd1$v[,1],col=numeric.activity)
plot(svd1$v[,2],col=numeric.activity)
legend(0.0,0.10,legend=unique(samsungData$activity),col=unique(numeric.activity),pch=19)

### Find Maximum contributor
plot(svd1$v[,2], pch=19)
max.contributor<- which.max(svd1$v[,2])
names(samsungData)[max.contributor]

### New Clustering with max.contributor
max.contributor<- which.max(svd1$v[,2])
distance.matrix <- dist(samsungData[samsungData$subject==1, c(10:12,max.contributor)])
hclustering <- hclust(distance.matrix)
plot(hclustering)
myplclust(hclustering,lab.col=numeric.activity)

### Look at variance via D matrix
par(mfrow=c(1,2))
plot(svd1$d, xlab="Column",ylab="Singular Value",pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Column",ylab="% of Variance Explained",pch=19)

### From 2nd plot above, the first singular value contributes close to 60% of variance
### What is max value of 1st right singlar vector cooresponding to this
max.contributor <- which.max(svd1$v[,1])
names(samsungData)[max.contributor]

############################################################
Video Lecture: Week 6, Prediction
############################################################
### Overfitting
set.seed(12345)
x <- rnorm(10); y<- rnorm(10); z <- rbinom(10,size=1,prob=0.5)
plot(x,y,pch=19,col=(z+3))
## Classifier and overtraining on training set
par(mfrow=c(1,2))
zhat <- (-0.2 < y) & (y < 0.6)
plot(x,y,pch=19,col=(z+3))
plot(x,y,pch=19,col=(zhat+3))
# New data for classifier overtraining example
set.seed(1233)
xnew <- rnorm(10); ynew <- rnorm(10); znew <- rbinom(10,size=1,prob=0.5)
par(mfrow=c(1,2)); zhatnew <- (-0.2 < ynew) & (ynew < 0.6)
plot(xnew,ynew,pch=19,col=(z+3)); plot(xnew,ynew,pch=19,col=(zhatnew+3))

#### Prediction of Non-Linear data using Trees
## Note that CART techniques are nonparametric decision tree techniques
## Use the iris dataset - inparticular Iris petal widths/sepal width
data(iris)
names(iris)
iris$Species
table(iris$Species)
plot(iris$Petal.Width, iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
legend(1,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

## the the tree library
library(tree)
tree1 <- tree(Species ~ Sepal.Width + Petal.Width,data=iris)
summary(tree1)
plot(tree1)
text(tree1)
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree1,label="Species",add=TRUE)
###
### Now let's generate new values and predict their species
set.seed(32313)
newdata <- data.frame(Petal.Width = runif(20,0,2.5),Sepal.Width= runif(20,0,4.5))
pred1 <- predict(tree1,newdata)
pred1
pred1 <- predict(tree1,newdata,type="class")
plot(newdata$Petal.Width,newdata$Sepal.Width,pch=19,col=as.numeric(pred1))
partition.tree(tree1,"Species",add=TRUE)
##partition.tree(tree1,  ordvars=c("Petal.Width","Sepal.Width"),add=TRUE)
str(pred1)

####
#### Pruning trees example: Cars
data(Cars93,package="MASS")
head(Cars93)
## Build a tree
tree.cars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags + EngineSize + Width + Length + Weight + Price + Cylinders +Horsepower + Wheelbase, data=Cars93)
plot(tree.cars)
text(tree.cars)
summary(tree.cars)
## Plot errors
par(mfrow=c(1,2))
plot(cv.tree(tree.cars,FUN=prune.tree,method="misclass"))
plot(cv.tree(tree.cars))
## Prune the tree
pruneTree <- prune.tree(tree.cars, best=4)
plot(pruneTree)
text(pruneTree)
summary(pruneTree)
############################################################
Video Lecture: Week 6, Video Reference: Cosma Shalizi Tree notes
############################################################
file.url <- "http://www.stat.cmu.edu/~cshalizi/350/lectures/21/calif_np.RData" 
download.file(file.url, destfile="./data/real_estate_ca.rda", method = "curl")
date.downloaded <- date()
load("./data/real_estate_ca.rda")
## Note: the california real estate dataframe is names "calif"
require(tree)
head(calif)
tree.fit <- tree(log(MedianHouseValue) ~ Longitude + Latitude, data=calif)
plot(tree.fit)
text(tree.fit)
price.deciles <- quantile(calif$MedianHouseValue, 0:10/10)
cut.prices <- cut(calif$MedianHouseValue,price.deciles,include.lowest=TRUE)
## Provide 2 plot functions below, one with grey scale and one with color
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,xlab="Longitude",ylab="Latitude")

## plot(calif$Longitude,calif$Latitude,col=as.numeric(cut.prices),pch=20,xlab="Longitude",ylab="Latitude")
partition.tree(tree.fit,ordvars=c("Longitude","Latitude"),add=TRUE)
summary(tree.fit)

## Now let's use all the variables in data set
tree.fit3 <- tree(log(MedianHouseValue) ~ .,data=calif)
summary(tree.fit3)
plot(tree.fit3)
text(tree.fit3)
## Unfortunately because income as well as geographical coordinates are use, we don't have a spatial partition to compare to previous ones, but we can map the predictions
## Predicted prices for the treefit3 model
cut.predictions <- cut(predict(tree.fit3), log(price.deciles),include.lowest=TRUE)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.predictions],pch=20,xlab="Longitude",ylab="Latitude")
