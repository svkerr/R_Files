### Torgo; Luis (2010-09-11). Data Mining with R 
library(DMwR)

### Explore the algae dataframe
str(algae)
summary(algae)
table(algae$season)
table(algae$size)

### Let's look at a particular attribute (variable): mxPH
hist(algae$mxPH)
hist(algae$mxPH,prob=T)
summary(algae$mxPH)

### Let's create combined histogram and density curve of mxPH
par(mfrow=c(1,1))
hist(algae$mxPH, prob=T,xlab=" ",main="Histogram of Max PH Value", ylim=0:1)
lines(density(algae$mxPH,na.rm=T))
rug(jitter(algae$mxPH))

### Let's create box-chart of oPO4
boxplot(algae$oPO4, ylab="Orthophosphate (oPO4)")
rug(jitter(algae$oPO4),side=2)
abline(h=mean(algae$oPO4, na.rm=T),lty=2)  ## Note use of na.rm=T here

### Let's look at how to identify an outlier
plot(algae$NH4,xlab="")
abline(h=mean(algae$NH4, na.rm=T),lty=1)
abline(h=mean(algae$NH4,na.rm=T) + sd(algae$NH4, na.rm=T),lty=2)
abline(h=median(algae$NH4,na.rm=T), lty=3)

### non-graphical way to identify outliers
algae[!is.na(algae$NH4) & algae$NH4> 5000,]

### Let's look at Conditioned Plots - graphical representations dependent upon a factor variable
library(lattice)
bwplot(size ~ a1, data = algae, ylab='River Size',xlab = 'Algal a1')

### How to deal with and investigate incomplete data sets
library(DMwR)
data(algae)
nrow(algae[complete.cases(algae),])
nrow(algae[!complete.cases(algae),])
nrow(algae)
not.complete <- algae[!complete.cases(algae),]

### To create a clean dataset using rows without NAs
algae.clean <- na.omit(algae)
nrow(algae.clean)

### Let's look for any correlation among the variables (columns)
cor(algae[,4:18],use = "complete.obs")
symnum(cor(algae[,4:18],use = "complete.obs"))
### The above plot slows a strong correlation between PO4 and oPO4 as noted by asterisk
### Let's develop the linear relationship
lm(PO4 ~ oPO4, data=algae)
### The results show that: PO4 = 42.897 * 1.293 oPO4
### We can use this linear relationship to fill in missing values of PO4

### We can try to explore the correlations between the variables with unknowns and the nominal 
### variables of this problem. We can use Conditioned Histograms that are available 
### through the lattice R package with this objective.
library(lattice)
histogram(~mxPH | season, data=algae)
histogram(~mxPH | size, data=algae)
histogram(~mxPH | size*speed, data=algae)

### We can also attempt to fill in missing values by investigating correlations
### among the observations -- will not do this here...

### Prediction - Obtaining Prediction Models
### We will explore two techniques: Multiple Linear Regression and Regression Trees
### manyNAs function is obtained from DMwR package
data(algae)
algae.sub <- algae[-manyNAs(algae),]
algae.clean <- knnImputation(algae,k=10)
nrow(algae.clean[!complete.cases(algae.clean),])  ### check to ensure we now have complete data
lm.a1 <- lm(a1 ~ ., data = algae.clean[,1:12])
summary(lm.a1)
### Given the R-squared of 0.3205, only about 32% of model variance is explained by these variables
### However, given the very small P-value, we cannot discount the model since it shows
### we should eliminate the null hypothesis of no relationships

### There are several methods to smplify regression models. Let's attempt to simplify the regression model. Techniaues include backward elimination...
### anova() will give us a sequential analysis of lessening the residual sum of squares
### (total model error) as we add predictor variables
anova(lm.a1)

### from this anova table, season is the covariate with the least contribution to variance explanation. So let's remove it from the model

lm2.a1 <- update(lm.a1, .~. - season)