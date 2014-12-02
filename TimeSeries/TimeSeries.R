setwd("/Users/stuart/R_Files/TimeSeries")
install.packages("psych")
library(xts)
library(zoo)
library(ggplot2)

data(AirPassengers)
AP <- AirPassengers
AP

# Maine unemployment data
Maine.month <- read.table("Maine.dat", header = T)
str(Maine.month)
class(Maine.month)
# Create a time series from the .dat file
Maine.month.ts <- ts(Maine.month$unemploy,start = c(1996,1), freq = 12)
layout(1:1)
plot(Maine.month.ts)
Maine.annual.ts <- aggregate(Maine.month.ts)/12
layout(1:2)
plot(Maine.month.ts)
plot(Maine.annual.ts)
Maine.Feb <- window(Maine.month.ts, start = c(1996,2),freq=TRUE)
plot(Maine.Feb)