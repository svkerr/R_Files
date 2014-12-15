## Investigations of Logistic Regression

setwd("/Users/stuart/R_Files/")
library(dplyr)
library(ggplot2)
library(MASS)    #important if doing confint of logistic regression
## ############Book: Extending the Linear Model with R ##################
library(faraway)
#### Binomial Data ######
data(orings)
plot(damage/6 ~ temp,orings)
lmod = lm(damage/6 ~ temp,orings)
abline(lmod)

logitmod = glm(cbind(damage,6-damage) ~ temp, family=binomial,orings)
summary(logitmod)

plot(damage/6 ~ temp,orings,xlim=c(25,85), ylim=c(0,1),
     xlab="Temperature",ylab="Prob of damage")
x = seq(25,85,1)
lines(x,ilogit(11.6630-0.2162*x))    # ilogit computes the inerse logit transformation

### Text: Logistic Regression Models; Hilbe
library(foreign)
library(Hmisc)
library(gmodels)
load("~/DataSets/LRM_R/heart01.rdata")
simple = data.frame(death=heart01$death,anterior=heart01$anterior[,drop=TRUE])
simple = na.omit(simple)
tsimple = ftable(simple)
CrossTable(simple$death,simple$anterior,dnn=c('death','anterior'))
# drop all other variables and factor levels except 'death' and 'anterior'
# compute logistic regression
fit1 = glm(death ~ anterior,data=simple,family=binomial(link='logit'))
summary(fit1)
exp(coef(fit1))     # odds ratios corresponding to intercept and anterior coef
confint(fit1)
exp(confint(fit1))  # odds ratio
# Predictions of probability
eta = coef(fit1)[1] + coef(fit1)[2]* 1    # predictor for anterior=1, this is log(odds)
# to go from log(odds) to probability (p) we transform log(p/(1-p)) = eta by exponentiating
p = 1/(1 + exp(-eta))   # p is probability
eta2 = coef(fit1)[1] + coef(fit1)[2]* 0    # predictor for anterior=0, this is log(odds)
p2 = 1/(1 + exp(-eta2))   # p is probability
# Get all fitted values
eta3 = predict(fit1,data.frame(anterior=simple$anterior),type='link',na.action=na.omit)
CrossTable(eta3)
head(eta3)
