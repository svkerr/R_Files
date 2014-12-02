# Practice area for R Programming
setwd("/Users/stuart/R_Files/")
library(dplyr)
library(ggplot2)

x = c(5,12,13)
for (n in x) {
  print(n^2)
}

i = 1
while (i <=10) i = i + 4
i

# Loop to read and print contents of two files
for(file in c("file1", "file2")) {
  print(scan(file))
}

## ############Book: Extending the Linear Model with R ##################
library(faraway)
data(gavote)
xtabs(~ atlanta + rural, gavote)

# Create some new features
gavote$undercount = (gavote$ballots - gavote$votes)/gavote$ballots
gavote$pergore = gavote$gore/gavote$votes

# Develop correlation matrix between quantitative variables
# Create vector of quantitative feature indices
nix = c(3, 10, 11,12)
cor(gavote[,nix])

# From that correlation matrix, let's investigate one of the stronger correlations
plot(pergore ~ perAA, gavote, xlab="Proportion African American")
# or using ggplot
ggplot(gavote,aes(perAA,pergore,colour=factor(rural))) + 
  geom_point() 

model1 = lm(pergore ~ perAA, gavote)
coef(model1)
predict(model1)    # Predicted or "fitted values" are yhat = X*Bhat
residuals(model1)  # residuals are eps = y -XBhat
deviance(model1)   # RSS 
confint(model1)
model1_summary = summary(model1)
model1_summary$sigma
attributes(model1_summary)

# Let's look at developing another linear model
model2 = lm(undercount ~ pergore + perAA, gavote)

# Let's look how well the model works
summary(model2)    #  Results in low p-value (reject null) but low R-Squared as well - likely accounted for dispersion around regression line
ggplot(gavote,aes(undercount,pergore)) + geom_point()    # dispersion about any regression line obvious from this plot
confint(model2)   # NOTE that the confint for both coefficients crosses zero --- i wouldn't use this regression model
predict(model2)    # Compute the predicted (fitted values)

# Let's construct another model, this time using categorical variables: rural (2 factors) and equip (5 factors)
ru = C(gavote$rural, treatment)
machine = C(gavote$equip, treatment)
gavote$cpergore = gavote$pergore - mean(gavote$pergore)
gavote$cperAA = gavote$perAA - mean(gavote$perAA)
model3 = lm(undercount ~ cperAA + cpergore*(ru) + (machine) ,gavote)
summary(model3)

# Comparison of two models using ANOVA
anova(model2,model3)
  
# Diagnostics
par(mfrow=c(2,2))
plot(model3)
par(mfrow=c(1,1)) 

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

############# Art of R Programming ##############################
oddcount = function(x) { 
  k = 0
  for (n in x)
    if(n %% 2 == 1) k = k+1
  k
}
oddcount(seq(1000))


## Define a measure of association between two vectors to the fraction of time x and y increase (or decrease) together
# This is similar to Kendall-tau statistic

# findud() converts vector v to 1s, 0s, representing an element increasing or not, relative to the previous one
# output length is one less than input
findud = function(v) {
  vud = v[-1] - v[-length(x)]
  return(ifelse(vud > 0,1,-1))
}
# udcor takes two vectors and correlates them based on findud()
udcor = function(x,y){
  ud = lapply(list(x,y),findud)
  return(mean(ud[[1]] == ud[[2]]))    
}

# Using diff() and sign() we can reduce findud() and udcor() into a one-liner
udcor2 = function(x,y) {
  return(mean(sign(diff(x)) == sign(diff(y))))
}
#######
grps = list()
for(gen in c("M","F","I"))
  grps[[gen]] = which(g == gen)



