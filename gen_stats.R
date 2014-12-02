### General statistics area
# Linear Regression and/or correlations
getwd()
library(ellipse)
library(ggplot2)


### Make some data
# X increases (noisly)
# Z increases slowly
# Y is constructed so it is inversely related to xvar and positively related to xvar*zvar

set.seed(955)
xvar = 1:20 + rnorm(20,sd=3)
zvar = 1:20/4 + rnorm(20,sd=2)  # remember colon is high priority operator
yvar = -2 * xvar + xvar*zvar/5 + 3 + rnorm(20,sd=4)

# Make data frame with variables
df = data.frame(x=xvar,y=yvar,z=zvar)
df

### Correlation coefficients
cor(df$x,df$y)
cor(df$x,df$z)

# Run correlations between many pairs of variables
cor(df)
round(cor(df),2)

# Neat way to visualize correlation matrix using library(ellipse)
ctab = cor(df)
plotcorr(ctab)

#### Linear regression
# These two commands will have the same outcome:
fit = lm(y ~ x, data=df)
fit = lm(df$y ~ df~x)
fit
# Get more detailed information
summary(fit)

# To help visualize linear regression using ggplot2
ggplot(df,aes(x,y)) + geom_point(shape=1) # basic plot of variables x and y

ggplot(df,aes(x,y)) +
  geom_point(shape=1) +   # use hollow circles
  geom_smooth(method=lm)  # includes confidence interval

ggplot(df,aes(x,y)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, se=FALSE)  # exclude shaded confidence region

ggplot(df,aes(x,y)) +
  geom_point(shape=1) +   # use hollow circles
  geom_smooth()  # includes Loess smoothed fit curve and confidence interval

## Interactions
# How to properly do multiple regression and test for interactions can be complex and not covered here (see Princeton class). We will fit a model with x, z and the interaction between the two

fit3 = lm(y ~ x * z, data=df)
fit3
summary(fit3)


## Example of Kernel Density Estimation (KDE)
library(KernSmooth)
attach(faithful)
fhat <- bkde(x=waiting)
plot (fhat, xlab="x", ylab="Density function")

## Simple linear regression example using core women data set
fit = lm(weight ~ height, data=women)
summary(fit)
# 99.1% of the variance in weight variable is accounted for by height variable

##### 
