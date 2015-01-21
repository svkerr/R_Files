# Analyzing SFPD data
# Set working directory where data files reside
setwd("/Users/stuart/DataSets/sanfran/Clean/sfpd_subset")

# Load key libraries
library(dplyr)
library(ggvis)
library(ggplot2)
library(lubridate)
library(reshape2)

# Read in data files

# If i only want to use one year (e.g., 2013) read in the file
dataset <-read.table("sfpd_incident_2013_copy.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

# If i want to read in multiple years (e.g., 3 years) read in the files
file_list = list.files()
file_list
for (file in file_list){
  if (exists("dataset")){
    temp_dataset <-read.table(file, header=TRUE, sep=",", stringsAsFactors = FALSE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  } else {
  # if the merged dataset doesn't exist, create it
    dataset <- read.table(file, header=TRUE, sep=",", stringsAsFactors = FALSE)
  }
}

# Check the dataset
str(dataset)

# Convert dataframe text entries to lower case
names(dataset) = tolower(names(dataset))
dataset$category = tolower(dataset$category)
dataset$descript = tolower(dataset$descript)
dataset$dayofweek = tolower(dataset$dayofweek)
dataset$pddistrict = tolower(dataset$pddistrict)
dataset$resolution = tolower(dataset$resolution)
dataset$location = tolower(dataset$location)

# convert dataset to dplyr table
sfpd = tbl_df(dataset)
glimpse(sfpd)
##########################################################
# select subset for mapping of crime exercise:
library(rgeos)
library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(mapproj)
library(ggmap)
library(knitr)
library(stats)
library(graphics)
library(grDevices)
library(utils)

crimes = c("assault","kidnapping","robbery")
crimes = c("assault","burglary","kidnapping","larceny/theft","robbery","vehicle theft")
# Pick a subset of crimes
sfpd_reduced = subset(sfpd, category == crimes)
sfpd_reduced$category = factor(sfpd_reduced$category, levels = crimes)

sfpd_reduced$incidntnum = NULL
sfpd_reduced$descript = NULL
sfpd_reduced$dayofweek = NULL
sfpd_reduced$date = NULL
sfpd_reduced$time = NULL
sfpd_reduced$pddistrict = NULL
sfpd_reduced$resolution = NULL
sfpd_reduced$location = NULL

# Restrict to around Financial District
#sfpd_reduced = subset(sfpd_reduced, -122.405400 <= x & x <= -122.396370 & 37.79600 <= y & y <= 37.79990)
# Map the results
SFMap <- qmap('san francisco', zoom = 14, color = 'bw',legend = 'topleft')
SFMap + geom_point(aes(x = x, y = y, size = category, colour = category), data = sfpd_reduced)

################Let's try ggmap() ##################################
sanfrancisco <- get_map('san francisco', zoom = 15) 
SFMap <- ggmap(sanfrancisco, extent = 'device', legend = 'topleft')

SFMap + stat_density2d(aes(x = x, y = y, fill = ..level.. , alpha = ..level..), size = 0, bins = 10, data = sfpd_reduced, geom = 'polygon') 
SFMap + stat_density2d(aes(x = x, y = y, colour=category), data = sfpd_reduced, bins= 5,geom = 'contour') 
SFMap + stat_density2d(aes(x = x, y = y, colour=category), data = sfpd_reduced, geom = 'polygon') 

scale_fill_gradient('Crime\nDensity') + scale_alpha(range = c(.2, .75), guide = FALSE) + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))

SFMap +  stat_density2d(aes(x = x, y = y, fill = ..level.., alpha = ..level..), size = 3, bins = 4, data = sfpd_reduced, geom = 'polygon')
+ scale_fill_gradient('Violent\nCrime\nDensity')
+ scale_alpha(range = c(.4, .75), guide = FALSE) 
+ guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))


# OPTIONAL Let's convert DayOfWeek to a factor
sfpd$dayofweek <- factor(sfpd$dayofweek, levels = c("sunday", "monday","tuesday","wednesday","thursday","friday","saturday"), labels = c("sun","mon","tue","wed","thr","fri","sat"))

# Let's do some visualization
# Barplots of categorical variables (category, pddistrict)
barplot(sort(table(dataset$category),decreasing=TRUE),  main="SF Crimes",las=3)
barplot(sort(table(dataset$pddistrict),decreasing=FALSE), main="Crimes per District",las=3)
barplot(sort(table(dataset$time),decreasing=FALSE), main="Crimes per Time",las=3)
barplot(sort(table(dataset$dayofweek),decreasing=FALSE), main="Crimes per Day",las=3)

####### Let's look at spatial distribution of crime #######
par(mfrow=c(1,2))
qplot(sfpd$x, data=sfpd, main="SF Crimes by Longitude",xlab="Longitude", geom="histogram")
qplot(sfpd$y, data=sfpd, main="SF Crimes by Latitude",xlab="Latitude",geom="histogram")
par(mfrow=c(1,1))

### Using standard hist(), 
# Side by side density histograms of crime by long and lat with normal dist using mean, sd
par(mfrow=c(1,2))
hist(sfpd$x, freq=FALSE, xlab="longtitude",ylim=c(0,35),main="Dist of Crime by Long",col="lightgreen")
curve(dnorm(x,mean=mean(sfpd$x), sd=sd(sfpd$x)),add=TRUE,col="darkblue",lwd=2)

hist(sfpd$y, freq=FALSE, xlab="latitude",ylim=c(0,35),main="Dist of Crime by Lat",col="lightgreen")
curve(dnorm(x,mean=mean(sfpd$y), sd=sd(sfpd$y)),add=TRUE,col="darkblue",lwd=2)
par(mfrow=c(1,1))

# Let's put two plots side-by-side: police district and longitude:
par(mfrow=c(1,2))
barplot(sort(table(dataset$pddistrict),decreasing=FALSE), main="Crimes per District",las=3)
hist(sfpd$x,main="SF Crimes by Longitude", xlab ="Longitude")     # longitude
par(mfrow=c(1,1))

# What this shows is that crime is not normally distributed (lat/long)
# and focused in Quadrant I of city

# Let's look at some numbers
sfpd %>%
  group_by(pddistrict, category) %>%
  summarise(crimes = n()) %>%
  arrange(desc(crimes))

sfpd %>%
  group_by(pddistrict) %>%
  summarise(crimes = n(), percentage = round(crimes/nrow(sfpd)*100,2)) %>%
  arrange(desc(crimes))

sfpd[-12] %>%
  group_by(dayofweek) %>%
  summarise(crimes = n()) %>%
  arrange(desc(crimes))

foo = sfpd[-12] %>%
  group_by(date) %>%
  summarise(crimes = n()) %>%
  arrange(desc(crimes))

sfpd[-12] %>%
  group_by(category) %>%
  summarise(crimes = n()) %>%
  arrange(desc(crimes))

crimesbytime = sfpd %>%
  group_by(time) %>%
  summarise(crimes = n()) %>%
  arrange(desc(crimes))

# Let's do some visualization using ggvis()
sfpd %>%
  group_by(pddistrict) %>%
  summarise(crimes = n()) %>%
  ggvis(~pddistrict, ~crimes) %>%
  layer_lines() %>%
  layer_points()

sfpd[-12] %>%
  group_by(category) %>%
  summarise(crimes = n()) %>%
  ggvis(~category, ~crimes) %>%
  layer_points() %>%
  layer_lines()

sfpd %>%
  group_by(category) %>%
  summarise(crimes = n()) %>%
  ggvis(~category, ~crimes) %>%
  layer_lines() 
 

####### Let's investigate the temporal distribution of crime #######
# We first put the dates in Posix format using lubridate:
sfpd$date = mdy(sfpd$date)
str(sfpd$date)
# Use ggplot2 - looks nicer
qplot(month(sfpd$date, label=TRUE), data=sfpd, main="SF Crimes by Month",xlab="Month",geom="histogram")
qplot(day(sfpd$date), data=sfpd, main="SF Crimes by Day of Month",xlab="Day",geom="histogram")
qplot(year(sfpd$date), data=sfpd, main="SF Crimes by Year", binwidth=1, xlab="Year",geom="histogram")

# Let's lubridate the time and create a new column with transformation
sfpd$hrmin = hm(sfpd$time)
qplot(hour(sfpd$hrmin), data=sfpd, main="SF Crimes by Hour of Day", binwidth=1,xlab="Hour",xlim=c(0,23),geom="histogram")
qplot(minute(sfpd$hrmin), data=sfpd, main="SF Crimes by Minute of Recorded Hour", binwidth=1,xlab="Minute",xlim=c(0,59),geom="histogram")

###### test of prototype red_oct algorithm - use of mutate with conditional statements ####
sfpd$hour <- hour(sfpd$hrmin)

sfpd_new <- sfpd %>%
  mutate(v5 = ifelse(hour==16 & (dayofweek=="saturday" | dayofweek=="monday"), 1, ifelse(hour == 22 & (dayofweek=="friday" | dayofweek=="wednesday"), 0, 5)))



#################################
####### Since "larceny/theft" is most frequent crime, let's create a separate DF for that category #######
sfpd_lar = subset(sfpd, category=="larceny/theft")

qplot(hour(sfpd_lar$hrmin), data=sfpd_lar, main="SF Larceny/Theft by Hour of Day", binwidth=1,xlab="Hour",xlim=c(0,23),geom="histogram")

m = ggplot(sfpd_lar,aes(x=hour(sfpd_lar$hrmin))) 
m + geom_histogram(aes(y=..density..),binwidth=.5,colour="black",fill="white") + 
  geom_density(alpha=.5,fill="#FF6666")

qplot(month(sfpd_lar$date, label=TRUE), data=sfpd_lar, main="SF Larceny/Theft by Month",xlab="Month",geom="histogram")

assault = subset(sfpd, category=="assault")
qplot(hour(assault$hrmin), data=assault, main="SF Assault by Hour of Day", binwidth=1,xlab="Hour",xlim=c(0,23),geom="histogram")


