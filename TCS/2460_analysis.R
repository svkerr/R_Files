#Read in 2460 file used at Level4 offsite in October 2013
data <- read.table("CCC_Detail2460", header = T, sep = ",")
# Scatter plots will primarily use ggplot2 package
library(ggplot2)
library(psych)
library(gclus)
library(rgl)

# Salary vs Years analysis
# Scatterplots
sp <- ggplot(data,aes(x=AERO_YR,y=SAL,colour=MANAGER))
sp + geom_point(size=2.5)
# now add some regression lines
sp + geom_point(size=2.5) + stat_smooth(method=lm, level=0.80)

# Density Plots
p<- ggplot(data, aes(x=AERO_YR,y=SAL))
p + geom_point() + stat_density2d()
p + stat_density2d(aes(colour=..level..))

p<- ggplot(data, aes(x=AERO_YR,y=SAL, colour=MANAGER))
p + geom_point() + stat_density2d()

# Degrees
# Histograms
ggplot(data,aes(x=DEG)) + geom_histogram() + facet_grid(MANAGER ~ .)
ggplot(data,aes(x=AERO_YR)) + geom_histogram() + facet_grid(DEG ~ .)
ggplot(data,aes(x=SAL)) + geom_histogram() + facet_grid(MANAGER ~ .)

# Analysis by Manager
# Create subsets
kerr<-subset(data,data[,23]=="KERR")
halford<-subset(data,data[,23]=="HALFORD")
kern<- subset(data,data[,23]=="KERN")

ggplot(data, aes(x=MANAGER,y=SAL)) + geom_boxplot() + stat_summary(fun.y="mean",geom="point",shape=23,size=2)

ggplot(kern,aes(x=SAL)) + geom_histogram()
ggplot(halford,aes(x=SAL)) + geom_histogram()


#### spare code.....#####
# or a different curve: loess = locally weighted polynomial
sp + geom_point(size=2.5) + stat_smooth(method=lm)
describeBy(data,data$MANAGER)