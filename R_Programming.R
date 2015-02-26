# Practice area for R Programming
## Text: Hands On Programming with R ###########################
setwd("/Users/stuart/R_Files/")
library(dplyr)
library(ggplot2)

# Dice simulation function
roll <- function() {
  die <- 1:6
  dice <- sample(die, size=2, replace=TRUE)
  sum(dice)  # last line in function must return a value
}

roll2 <- function(die = 1:6){   # Note that an argument assignment must be an '=' sign (default values)
  dice <- sample(die, size=2, replace=TRUE)
  sum(dice)
}

## Now let's experiment with the dice simulator
z <- 1:1000
foo = vector()
for(i in z){ 
  foo <- c(foo, roll2())
}
foo
qplot(foo, binwidth=1)

## Instead of a for() loop, let's use replicate()
rolls <- replicate(10000,roll())
qplot(rolls, binwidth=1)

## Let's weight the number 6 with a 3/8 probability
## This vector will be fed to sample() within our function
weights <- c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8)  # weights is an atomic vector
roll_weighted <- function() {
  die <- 1:6
  dice <- sample(die, size=2, replace=TRUE, prob=weights)
  sum(dice)  # last line in function must return a value
}
rolls <- replicate(10000,roll_weighted())
qplot(rolls, binwidth=1)

### Part II Playing Cards
# Design a deck of playing cards that we can shuffle and deal from
die <- 1:6
names(die)
names(die) <- c("one", "two", "three", "four", "five", "six")
die
names(die)
attributes(die)
# to remove the attribute of names, set it to null:
names(die) <- NULL



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



