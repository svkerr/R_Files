pcard <- read.csv("tcsPcardExpenses.csv")
##########################################
str(arrakisIdHours)
boxplot(arrakisIdHours$cores)
str(arrakisIdHours)
red.wallclock <- subset(arrakisIdHours,subset = (arrakisIdHours$wallclock < 20000))
boxplot(red.wallclock$wallclock, range=0)