# Analyzing SFPD data
setwd("/Users/stuart/R_Files/SFPD")
data <- read.table("sfpd_redux_cols_lc", header = T, sep = ",")

str(data)
summary(data$date)
summary(data$pddistrict)

plot(data$pddistrict)
plot(data$dayofweek)
plot(data$category)
max(data$category)