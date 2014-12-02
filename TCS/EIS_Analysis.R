# Analysis of EIS <= L3 data
# File obtained on 30 October 2013

# Set the working directory
setwd("/Users/stuart/R_Files/TCS")

# Load required libraries
library(psych)
library(ggplot2)
library(gclus)
library(rgl)

# Read in spreadsheet
EIS <- read.table("FY14SalaryData9-30-13_ver2.csv",header = T, sep = ",")

# Do some verification with original spreadsheet
str(EIS)
describe(EIS)

### GENERAL PLOTS  ###

# Get general plot of Salary versus Aerospace Years and color by degree
sp <- ggplot(EIS,aes(x=AERO_YR,y=SAL,colour=DEG))
sp + geom_point(size=4.0)

# Get general plot of Salary versus Aerospace Years and color by Org Level
sp <- ggplot(EIS,aes(x=AERO_YR,y=SAL,colour=ORG_LVL))
sp + geom_point(size=4.0)

# Get general plot of Org Level versus Aerospace Years, colored by Degree
sp <- ggplot(EIS,aes(x=AERO_YR,y=ORG_LVL, colour=DEG))
sp + geom_point(size=4.0)

# Get general plot of Over_Under the MaxSal based on Years
sp <- ggplot(EIS,aes(x=AERO_YR,y=OVER_UNDER, colour=DEG))
sp + geom_point(size=4.0)

# Create subcategories based on ORG_LEVEL
L3<-subset(EIS,EIS[,4]==3)
L2<-subset(EIS,EIS[,4]==2)
L1<-subset(EIS,EIS[,4]==1)
L0<-subset(EIS,EIS[,4]==0)

# Plot salary histograms of these ORG_LEVEL subgroups and look for normal distributions
ggplot(L3,aes(x=SAL)) + geom_histogram()
ggplot(L2,aes(x=SAL)) + geom_histogram()
ggplot(L1,aes(x=SAL)) + geom_histogram()
ggplot(L0,aes(x=SAL)) + geom_histogram()

sp <- ggplot(EIS,aes(x=SAL,y=ORG_LVL,colour=DEG))
sp + geom_point(size=4.0)

# Box plot of Deg versus Salary
ggplot(EIS, aes(x=DEG,y=SAL)) + geom_boxplot() + geom_jitter() + geom_rug(position='jitter')+ stat_summary(fun.y="mean",geom="point",shape=23,size=2)

### CORRELATION ANALYSIS ###

SOA <- subset(EIS,select=c(SAL,ORG_LVL,AERO_YR))
cor(SOA[1:3])

### REGRESSION ANALYSIS ####
# Regression on Salary being modeled by Years at Aerospace
# Note: low R-squared and high p-value -> years is meaningless in predicting EIS salaries
cor(EIS[4,7,12])
model1 <- lm(EIS$SAL ~ EIS$AERO_YR)
summary(model1)

# Regression on Org Level being modeled by Years at Aerospace
# Note: low R-squared and extremely high p-level demonstrates that years is an extremely poor predictor of Org Level in EIS
model2 <- lm(EIS$ORG_LVL ~ EIS$AERO_YR)
summary(model2)

# Regression on Salary being modeled by Degree
## We need to use dummy codes to represent the nominal variable (DEG) as numeric
deg.code <- C(EIS$DEG, treatment)  # Note that AA becomes our reference due to alphabetical order
deg.code
model3 <- lm(EIS$SAL ~ (deg.code))
summary(model3)

# Look at some general Salary means across variables
tapply(EIS$SAL, EIS$DEG, mean)
tapply(EIS$SAL, EIS$CCC, mean)
tapply(EIS$SAL, EIS$ORG_LVL, mean)


