pcard <- read.csv("tcsPcardExpenses.csv")
##########################################
str(arrakisIdHours)
boxplot(arrakisIdHours$cores)
str(arrakisIdHours)
max(arrakisIdHours$wallclock)
red.wallclock <- subset(arrakisIdHours,subset = (arrakisIdHours$wallclock < 200000))
boxplot(red.wallclock$wallclock, range=0)
rug(jitter(red.wallclock$wallclock),side=2)
abline(h=mean(red.wallclock$wallclock),lty=2)

## set up various reduced wallclock times populations
red200k.wallclock <- subset(arrakisIdHours,subset = (arrakisIdHours$wallclock < 200000))
red100K.wallclock <- subset(arrakisIdHours,subset = (arrakisIdHours$wallclock < 100000))
red50K.wallclock <- subset(arrakisIdHours,subset = (arrakisIdHours$wallclock < 50000))
red5K.wallclock <- subset(arrakisIdHours,subset = (arrakisIdHours$wallclock < 5000))

boxplot(red50K.wallclock$wallclock, range=0)
rug(jitter(red50K.wallclock$wallclock),side=2)
abline(h=mean(red50K.wallclock$wallclock),lty=2)
## Based on these calculations, > 98% of processes last under 50K seconds

## Boxplot of job id's lasting less than or equal to 5k seconds
boxplot(red5K.wallclock$wallclock, range=0)
rug(jitter(red5K.wallclock$wallclock),side=2)
abline(h=mean(red5K.wallclock$wallclock),lty=2)
###############################################
###Manticore Data##############################
mcore <- read.csv("manticore.csv")