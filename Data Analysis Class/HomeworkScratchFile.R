# Scratch space used when taking Quizzes

set.seed(31)
heightsCM <- rnorm(30, mean=188, sd=5)
weightsK <- rnorm(30, mean=84, sd=3)
hasDaughter <- sample(c(TRUE,FALSE),size = 30, replace=T)
dataFrame <- data.frame(heightsCM, weightsK, hasDaughter)

## New question
set.seed(41)
cauchyValues <- rcauchy(100)

## New question
set.seed(415)
sampleCauchy <- sample(cauchyValues, 10, replace = TRUE)

for(i in 1:5) {
  fileName <- paste0("./data",i,".csv")
  print(fileName)
}

# Working area from Week 2 Slides - Read a .csv file
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./data/cameras.csv", method="curl")
dateDownloaded <- date()
cameraData <- read.csv("./data/cameras.csv")

# Working area from Week 2 Slides - Read a .xlsx file using xlsx2 (package: xlsx)
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./data/cameras.xlsx", method="curl")
dateDownloaded <- date()
cameraData <- read.xlsx2("./data/cameras.xlsx", sheetIndex=1)

# readLines() from the Web
con <- url("http://simplystatistics.org", "r")
simplyStats <- readLines(con)
close(con)
############################
# Earthquake Data
fileUrl <- "http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M1.txt"
download.file(fileUrl, destfile="./data/earthquakeData.csv", method="curl")
dateDownLoaded <- date()
edata <- read.csv("./data/earthquakeData.csv")
###################
# Quiz 2, Question 2
con <- url("http://simplystatistics.tumblr.com/", "r")
simplyStats <- readLines(con, n = 150)
close(con)
######################
#Quiz 2, Question 3
fileUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/ss06hid.csv"
download.file(fileUrl, destfile="./data/community.csv", method = "curl")
dateDownLoaded <- date()
commdata <- read.csv("./data/community.csv")

milvalue <- subset(commdata, subset = (commdata$VAL == 24))

#######################
#Quiz 2, Question 5
# Variables: Bedrooms: BDS     Total Rooms: RMS
# Assumes that commdata dataframe is loaded
roomvalue <- subset(commdata, subset = (commdata$BDS == 3 & commdata$RMS == 4))
roomvalue <- subset(commdata, subset = (commdata$BDS == 2 & commdata$RMS == 5))
roomvalue <- subset(commdata, subset = (commdata$BDS == 2 & commdata$RMS == 7))

#######################
#Quiz 2, Question 6
# Variables: Acres: ACR==3     Agr Sales: AGS==6
# Assumes that commdata dataframe is loaded
commdata$ACR == 3
agriculturalLogical <- commdata$ACR == 3 & commdata$AGS == 6
which(agriculturalLogical)

##########################
#Quiz 2, Question 7
# Variables: Acres: ACR==3     Agr Sales: AGS==6
# Assumes that commdata dataframe is loaded
agriculturalLogical <- commdata$ACR == 3 & commdata$AGS == 6
indexes <- which(agriculturalLogical)
subsetcommdata <- commdata[indexes,]
sum(is.na(subsetcommdata$MRGX))
###############################
#Quiz 2, Question 8
# Assumes that commdata dataframe is loaded
splitNames <-  strsplit(names(commdata),"wgtp")
splitNames[123]
###############################
#Quiz 2, Question 9
#Variable YBL
quantile(commdata$YBL, na.rm = TRUE)
################################
#Quiz 2, Question 10
housingData <- milvalue
fileUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/ss06pid.csv"
download.file(fileUrl, destfile="./data/population.csv", method = "curl")
dateDownLoaded <- date()
populationData <- read.csv("./data/population.csv")
mergedData <- merge(housingData, populationData, by="SERIALNO")
###############################
#Quiz 3, Question 4
iris.subset <- subset(iris, select=c(Sepal.Length,Sepal.Width,
                                     Petal.Length,Petal.Width))
iris.clustering <- dist(iris.subset)
iris.clustering <- hclust(iris.clustering)
plot(iris.clustering)
# Answer is if i cut the tree at height 3, i have 4 clusters
###############################
#Quiz 3, Question 3
library(ElemStatLearn)
data(marketing)
plot(bone$age,bone$spnbmd,pch=19,col=((bone$gender=="male")+1))
boxplot(marketing$Income ~ marketing$Marital,col="grey",xaxt="n",ylab="Income",xlab="")
axis(side=1,at=1:5,labels=c("Married","Living together/not married","Divorced or separated","Widowed","Nevermarried"),las=2)
################################
#Quiz 3, Question 4
file.url <- "https://spark-public.s3.amazonaws.com/dataanalysis/quiz3question4.csv" 
download.file(file.url, destfile="./data/quiz3ques4.csv", method = "curl")
date.downloaded <- date()
quiz3.ques4 <- read.csv("./data/quiz3ques4.csv")
plot(quiz3.ques4$x,quiz3.ques4$y)
kmeans.result <- kmeans(quiz3.ques4, 2)
str(kmeans.result)
## Do some plotting of clustering results - in 2-dim euclidean space
plot(quiz3.ques4[c("x", "y")], col = kmeans.result$cluster)
## Plot cluster centers
points(kmeans.result$centers[,c("x", "y")], col =  1:2, pch=8, cex=2)
#################################