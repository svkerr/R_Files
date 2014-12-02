##### Data Files Area#####
file.url <- "https://d396qusza40orc.cloudfront.net/introstats/Data%2FLifeExpTable.txt" 
download.file(file.url, destfile="./data/lifeExpect.txt", method = "curl")
date.downloaded <- date()
data = read.table("./data/lifeExpect.txt")

file.url <- "https://d396qusza40orc.cloudfront.net/introstats/Data%2FSkeletonData.txt" 
download.file(file.url, destfile="./data/Skeleton.txt", method = "curl")
date.downloaded <- date()
skeleton = read.table("./data/Skeleton.txt", header=TRUE)


file.url <- "https://d396qusza40orc.cloudfront.net/introstats/Data%2FNYRedBullsSalaries.txt"
download.file(file.url, destfile="./data/redBullSalary.txt", method = "curl")
date.downloaded <- date()
nysalaries = read.table("./data/redBullSalary.txt")

file.url <- "https://d396qusza40orc.cloudfront.net/introstats/R%20tutorials%2FLifeExpRegion.txt"
download.file(file.url, destfile="./data/LifeExpRegion.txt", method = "curl")
data.downloaded <- date()
data = read.table("./data/LifeExpRegion.txt")

file.url <- "https://d396qusza40orc.cloudfront.net/introstats/R%20tutorials%2FLifeGDPhiv.txt"
download.file(file.url, destfile="./data/LifeGDPhiv.txt", method = "curl")
data.downloaded <- date()
Countries = read.table("./data/LifeGDPhiv.txt")
#############################
## Video Scripting Area ##
grades <- c(79,68,69,88,90,74,87,76,93)
summary(grades)
boxplot(grades, ylab='Grades',ylim=c(60,100))
rug(jitter(grades),side=2)
##############################
## Video Assignments Week 2
skeleton = read.table("./data/Skeleton.txt", header=TRUE)
colnames(skeleton)<- c('Obs','Sex','BMI','Age','DGEstimate','DGDifference')
attach(skeleton)
male.skel<- skeleton[Sex == 'Male',]
fem.skel <- skeleton[Sex == 'Female',]
boxplot(male.skel[,6], fem.skel[,6], range=0,border=rainbow(2),names=c('Male','Female'))
boxplot(skeleton~BMI,range=0)