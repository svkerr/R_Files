## Investigations of Logistic Regression

setwd("/Users/stuart/R_Files/")
library(dplyr)
library(ggplot2)
library(MASS)    #important if doing confint of logistic regression
## ############Book: Extending the Linear Model with R ##################
library(faraway)
#### Binomial Data -- O-Ring example ####################################
data(orings)
plot(damage/6 ~ temp,orings)
lmod = lm(damage/6 ~ temp,orings)
abline(lmod)

logitmod = glm(cbind(damage,6-damage) ~ temp, family=binomial(link=logit),orings)
summary(logitmod)

plot(damage/6 ~ temp,orings,xlim=c(25,85), ylim=c(0,1),
     xlab="Temperature",ylab="Prob of damage")
x = seq(25,85,1)
#lines(x,ilogit(11.6630-0.2162*x))    # ilogit computes the inerse logit transformation
lines(x,1/(1 + exp(-(11.66330 - 0.2162 * x))))
# Let's predict the response at 31deg Farenheit
pred31 = coefficients(logitmod)[1] + coefficients(logitmod)[2] * 31

# NOTE: If the Deviance far exceeds the degrees of freedom the null hypothesis can be rejected. Given that our model's residual deviance 16.9 is less than the DOF of 21, we don't reject this model

# Inferences of above model - using pchisq (Chi-Square Test)
# since the above p-value is far greater than 0.05, we conclude model fits sufficiently well.
pchisq(deviance(logitmod),df.residual(logitmod), lower=FALSE)
# but same test on the null model (only includes the intercept,excludes temp predictor), we see model is not sufficient (reject the null)
pchisq(38.9,22,lower=FALSE)

# We can test the significance of the temperature by computing the difference in the deviances between the model with and without temperature. The model w/o temperature is just the null model and the difference in desgrees of freedom or paramters is just 1:

pchisq(38.9 - 16.91, 1, lower=FALSE)
# Since above p-value is far less that .05, we conclude that temperature variable is significant

# Lastly, let's calculate the confidence intervals for the model's coefficients:
confint(logitmod)

# We can test for dispersion by calculating the Chi-Sq statistic (sum of squares of Pearson residuals over residuals DOF)
sum(residuals(logitmod,type="pearson")^2)/df.residual(logitmod)
# this result shows there is some dispersion

# Create new orings data set
orings2 = orings
orings2[1,2]= 1
str(orings2)
logitmod2 = glm(cbind(damage,1-damage) ~ temp, family=binomial(link=logit),orings2)
summary(logitmod2)
# Check for dispersion
sum(residuals(logitmod2,type="pearson")^2)/df.residual(logitmod2)
# Note: there is less disperson than logitmod

# Let's predict probability of failure at 31 degrees
pred31 = coefficients(logitmod2)[1] + coefficients(logitmod2)[2] * 31
prob_of_fail31 = 1/(1 + exp(-pred31))
#Note: this probability is higher than that of previous logitmod model

# Let's try and plot
plot(damage ~ temp,orings,xlim=c(25,85), ylim=c(0,1),xlab="Temperature",ylab="Prob of damage")
x = seq(25,85,1)
#lines(x,ilogit(11.6630-0.2162*x))    # ilogit computes the inerse logit transformation
lines(x,1/(1 + exp(-(coefficients(logitmod2)[1] + coefficients(logitmod2)[2] * x))))

#### Binomial Data -- BabyFood example ###################################
data(babyfood)
glimpse(babyfood)
xtabs(disease/(disease + nondisease) ~ sex + food,babyfood)
mdl = glm(cbind(disease,nondisease) ~ sex + food, family = binomial(link=logit), babyfood)
summary(mdl)
# Test significance of main effects, where drop1 function tests each parameter relative to the full
drop1(mdl,test="Chi")
exp(coefficients(mdl))
# We see both predictors are significant in this sense



### Text: Logistic Regression Models; Hilbe #############################
library(foreign)
library(Hmisc)
library(gmodels)
load("~/DataSets/LRM_R/heart01.rdata")
load("~/DataSets/LRM_R/heartr.rdata")
load("~/DataSets/LRM_R/heart02grp.rdata")
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

### Residual Analysis
# Using heartr data
fit7_4a = glm(death ~ anterior + hcabg + kk2 + kk3 + kk4 + age3 + age4,data=heartr,family=binomial(link='logit'))
# Let's calculate mu = fitted value from the logistic regression. Read predict() func explanation. Basically, for logistic regression, type=response returns the probabilities (p or mu) instead of log(odds) value
mu = predict(fit7_4a, type='response')
simple_heartr = na.omit(heartr)    # need to remove NAs from heartr for next calculation (though the glm() automatically removed NAs during the regression phase using heartr)
raw = simple_heartr$death - mu    # Calculate the raw residual (rarely used in analysis)
head(cbind(simple_heartr$death,mu,raw))

# Calculate Pearson residuals
pearson = residuals(fit7_4a, type='pearson')
variance = mu * (1-mu)
head(cbind(simple_heartr$death,mu,raw,variance,pearson))
chi = sum(pearson * pearson)
# Calculate p-value from this test (two different ways)
1 - pchisq(chi,df= df.residual(fit7_4a))
pchisq(deviance(fit7_4a),df.residual(fit7_4a), lower=FALSE)

# Given the very high value above, we cannot reject the null (that is good)

devianze = residuals(fit7_4a, type='deviance')
head(cbind(simple_heartr$death,mu,devianze))
dev_gof = sum(devianze * devianze)
dev

# The standardized deviance residual is one of the central residuals used fo rgraphing GLM-based regression models. A scatterplot of the standardized deviance by the fitted value, mu is the best graphical method of assessing the internal shape of the modeled data from among the standard GLM-based residuals.
sd_deviance = rstandard(fit7_4a)   # standardize deviance residuals
head(cbind(devianze, sd_deviance))

sd_deviance_2 = sd_deviance * sd_deviance
summary(sd_deviance_2[simple_heartr$death==1])
summary(sd_deviance_2[simple_heartr$death==0])


########## Text: Beginner's Guide to GLM and GLMM with R #################
bee = read.csv("/users/Stuart/DataSets/GLM_GLMM_R/workerbees.csv",sep=",", header=TRUE)
plot(bee$CellSize,bee$Parasites)
