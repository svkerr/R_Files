setwd("/users/stuart/R_Files/OpenIntroStatsClass")
my_url = "http://www.openintro.org/stat/data/present.R"
present = source(my_url)
present

data = present$value
data
plot(x = data$year, y = (data$boys - data$girls)
)
plot(x = data$year, y = (data$boys - data$girls), type='l')
plot(x = data$year, y = data$girls)
plot(x = data$year, data$boys + data$girls, type = 'l')
data[1:12]
data
data[1:30]
data[1:30][2]
##### Lab 1 Starts Here #########
## NOTE: since the DF comes in with a $value parameter, we convert that to our real DF
cdc_data = source("http://www.openintro.org/stat/data/cdc.R")
cdc = cdc_data$value
cdc
str(cdc)
dim(cdc)
names(cdc)
poor_index = which(cdc$genhlth == 'poor')
poorDF = cdc[poor_index,]
summary(poorDF)
mean(cdc$weight)
# to see weights of 1st 10 respondants:
cdc[1:10,6]
# To get summaries of categorical variables, we use 'table'
table(cdc$smoke100)
table(cdc$smoke100)/20000
barplot(table(cdc$smoke100))

summary(cdc$gender)

table(cdc$genhlth)/20000

gender_smokers = table(cdc$gender, cdc$smoke100)
mosaicplot(gender_smokers)
