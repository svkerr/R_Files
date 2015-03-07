# General test area for new R things
# Libraries for jsonlite demo
library(nycflights13)
library(jsonlite)

# Libraries for Wickham's shape file viewer
library(rgeos)
library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(mapproj)
library(ggmap)

getwd()

#### jsonlite demo ####################
stream_out(flights, con = file("~/flights.json"))
flights3 = stream_in(file("flights.json"))

##### Wickham shape file demo ###################
setwd("/Users/stuart/DataSets/ecoregion_design")
utah = readOGR(dsn=".", layer="eco_l3_ut")
#slotNames(utah)
utah@data$id = rownames(utah@data)

utah.points = fortify(utah, region="id")
utah.df = join(utah.points, utah@data, by="id")

ggplot(utah.df) + 
  aes(long,lat,group=group,fill=LEVEL3_NAM) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_brewer("Utah Ecoregion")


# Let's try Pakistan
setwd("/Users/stuart/DataSets/pakistan-latest.shp")
paki = readOGR(dsn=".", layer="railways")
paki@data$id = rownames(paki@data)
paki.points = fortify(paki, region="id")
paki.df = join(paki.points, paki@data, by="id")

ggplot(paki.df) + 
  aes(long,lat,group=group,fill=type) + 
  geom_polygon() +
  geom_path(color="red") +
  coord_equal() 

### Google Image Data using ggmap() ####################
library(ggmap)
# Let's plot san francisco via a bounding box
sf = c(-122.512959,37.709097,-122.395200,37.811424)
# Read in K-Means determined centers of crime
sfpd_centers = read.table("/Users/stuart/DataSets/sanfran/sfpd_centers.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
# ggmap it
sf.map = get_map(location = sf,
color = "color",
source = "google",
maptype = "hybrid",
zoom = 12)

foo = ggmap(sf.map,
      extent = "panel",
      ylab = "Latitude",
      xlab = "Longitude") 
foo + geom_point(data=sfpd_centers,aes(x=long,y=lat),colour='red',size=3.0)


map("world", "Pakistan")
map.cities(country = "Pakistan", capitals = 1)

map("state", "VIRGINIA")
data(us.cities)
data(county.fips)
map.cities(us.cities, country = "VA")
data(countyMapEnv)

map('county', 'virginia', names=TRUE, fill=FALSE,plot=TRUE)
map('county', 'c=(virginia,buchanan,carroll)', names = TRUE, plot = TRUE)


library(maps)
m <- map("county", "virginia", plot=FALSE)
names(m)
m$names
map.text('county','virginia', cex=0.5, mar = c(4.1, 4.1, par("mar")[3], 0.1))
map.text('county','virginia', cex=0.5, mar = c(4.1, 4.1, 4.1, 4.1))
map.text('county','virginia', proj='bonne', cex=0.5, param=20, labels=paste(1:length(m$names)))

m$my.colors <- 0
m$my.colors[m$names %in% c("buchanan","carroll","dickenson","henry")] <- "red"
map("county", "virginia", boundary = TRUE, col=m$my.colors, add = TRUE, fill=TRUE, plot=TRUE )


library(maps)
map('usa')

map("state","VIRGINIA")
map('county', 'virginia', add=T, fill = T, col=c(1:5), plot=TRUE) 
plots Colorado counties using colours 1 to 5. 






# Plot unemployment by country
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", 
           "#980043")
head(unemp)
head(county.fips)
str(county.fips)
county.fips$polyname == "virginia"

######### Boosting Algorithms ###########
# Paper Reference: "Model-based Boosting in R" - Hofner et.al.,
library("mboost")
library("TH.data")  # We will use the bodyfat data set
library(dplyr)
lm1 = lm(DEXfat ~ hipcirc + kneebreadth + anthro3a, data=bodyfat)
lm2 <- lm(DEXfat ~., data=bodyfat)

coef(lm1)
summary(lm1)
summary(lm2)
plot(lm1$residuals)

# Now let's try boosting with same predictors as lm1
glm1 <- glmboost(DEXfat ~ hipcirc + kneebreadth + anthro3a, data=bodyfat)
coef(glm1, off2int=TRUE)   # off2int adds the offset to the intercept
coef(glm1)

# Let's use all predictors -- using paste to set up formula
preds <- names(bodyfat[, names(bodyfat) != 'DEXfat'])
fm <- as.formula(paste('DEXfat~', paste(preds,collapse='+')))
glm2 <- glmboost(fm, data=bodyfat, center=TRUE)
coef(glm2, which='')  # The coef() by default only displays the selected variables, but can be forced to show all effects by specifying which=''
mstop <- mstop(AIC(glm2))
boost_control(mstop=mstop(AIC(glm2)))
attributes(AIC(glm2))
coef(glm2[mstop(AIC)])
glm3 <- glmboost(fm, data=bodyfat, control=boost_control(mstop=45), center=TRUE)
coef(glm3)
coef(glm2)

# Now boost applied to Logistic Regression
# Wisconsin prognostic breast cancer. We first analyze this data as a binary prediction problem (recurrence vs. non-recurrence). We have many co-variates (we'll exclude time) with few observations. Thus, variable selection is an important issue. 
# load data
data(wpbc)
glimpse(wpbc)
nrow(wpbc); ncol(wpbc)
# We can follow a classical logistic regression model via AIC in a stepwise algorithm as follows:

# remove missing values and time covariate
cc <- complete.cases(wpbc)
wpbc2 <- wpbc[cc, colnames(wpbc) != 'time']
# fit logistic regression model
wpbc_nostep <- glm(status ~ ., data=wpbc2, family=binomial)
wpbc_step <- step(glm(status ~ ., data=wpbc2, family=binomial()), trace=0)
summary(wpbc_step)
summary(wpbc_nostep)
attributes(wpbc_step)
pseudoR_step <- 1 - (wpbc_step$deviance/wpbc_step$null.deviance); pseudoR_step
pseudoR_nostep <- 1 - (wpbc_nostep$deviance/wpbc_nostep$null.deviance); pseudoR_nostep
logLik(wpbc_step) # The final model consists of 16 parameters
# We want to compare this model with a logistic regression model fitted via gradient boosting.
# Fit logistic regression model via gradient boosting
ctrl <- boost_control(mstop=500)
wpbc_glm <- glmboost(status ~ ., data=wpbc2, family=Binomial(), center=TRUE, control=ctrl)
aic <- AIC(wpbc_glm, 'classical'); aic

# We now restrict the number of boosting iterations to mstop=260 and obtain the estimated coefficients
# Fit with new mstop
wpbc_glm <- wpbc_glm[mstop(aic)]
coef(wpbc_glm)
attributes(wpbc_glm)
plot(wpbc_glm$resid)
