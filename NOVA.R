# Project: NOVA
# Analyzing Virginia data

# Set working directory where data files reside
setwd("/Users/stuart/DataSets/virginia/")

# Load key libraries
library(dplyr)
library(ggvis)
library(ggplot2)
library(lubridate)
library(reshape2)
library(plyr)

# Read in data files
pop <-read.table("population.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
commute <-read.table("commute_2010_00.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
ces <-read.table("ces.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
ind2013 <-read.table("Industry/2013-00.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE,strip.white = TRUE)
indsize2013 <- read.table("IndustryBySize/2013-00.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE,strip.white = TRUE)
led2013_01 <- read.table("LED/2013-01.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE,strip.white = TRUE)
bridges = read.csv("bridge_inspections.csv", header=TRUE, quote="", row.names=NULL, stringsAsFactors = FALSE,strip.white = TRUE)  # special parameters set for this file
inc = read.table("income.txt",header=TRUE, sep="\t",stringsAsFactors = FALSE, strip.white = TRUE)
labor = read.table("labforce.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE, strip.white=TRUE)
suicide = read.table("virginiaSuicideDataSet.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, strip.white=TRUE)

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

# Convert df's to dplyr format
commute = tbl_df(commute)
pop = tbl_df(pop)
ces = tbl_df(ces)
ind2013 = tbl_df(ind2013)
indsize2013 = tbl_df(indsize2013)
led2013_1 = tbl_df(led2013_01)
bridges = tbl_df(bridges)
suicide = tbl_df(suicide)

###### Commute Dataset ##########################################
# Let's isolate virginia workers into multiple df's.
# Those that work in the state
# Those that live and work in state
# Those that work in state (but can include commuters from out of state) 
# Ugh, limited to one year: 20120

commute %>%
  filter(wkstatename == "Virginia") %>%
  summarise(sum(workers))

commute %>%
  filter(statename == "Virginia" & wkstatename == "Virginia") %>%
  summarise(sum(workers))

commute %>%
  filter(statename != "Virginia" & wkstatename == "Virginia") %>%
  summarise(sum(workers))

# Number of workers living and working in VA
summarise(va_livework, workers = sum(workers))

# States supplying workers to Virginia, ranked by number of workers
va_commwork <- commute %>%
  filter(statename != "Virginia" & wkstatename == "Virginia")

va_commwork %>%
  group_by(statename) %>%
  summarise(count = n(), num_workers = sum(workers)) %>%
  arrange(desc(num_workers))

###### Population Dataset ##########################################
# Restrict population analyis to just counties and not cities as well (which get included with 'areaname')
pop <- subset(pop, grepl(" County", pop$areaname, perl=TRUE))
# Create some dataframe subsets to analyze

fairfax_pop <- pop %>%
  filter(areaname == "Fairfax County" & statename == "Virginia" & periodtype ==1) 

chesterfield_pop <- pop %>%
  filter(areaname == "Chesterfield County" & statename == "Virginia" & periodtype ==1) 

ggplot(fairfax_pop, aes(x=periodyear, y=population)) + 
  geom_point(colour="red") +
  labs(x="Year", y="Population", title = "Fairfax County Population")

ggplot(chesterfield_pop, aes(x=periodyear, y=population)) + 
  geom_point(colour="red") +
  labs(x="Year", y="Population", title = "Chesterfield County Population")

virginia.pop <- pop %>%
  select(statename,areatyname,areaname,periodyear,periodtype, population) %>%
  filter(areatyname == "County" & statename == "Virginia" & periodtype ==1)

ggplot(virginia.pop, aes(periodyear, population, group=areaname)) + 
  geom_line() +
  labs(x="Year", y="Population", title = "Annual Growth of Virginia Counties")

ggplot(virginia.pop, aes(x=periodyear, y=population, colour=areaname,group=areaname)) +
  geom_line() +
  theme(legend.position="none") +
  ggtitle("Annual Population Growth Curves of Virginia Counties")


# From the above plot, let's figure out which ones have population > 300,000 in 2010 excluding Fairfax
growers <- virginia.pop %>%
  filter(population > 300000) %>%
  select(areaname, periodyear, population) %>%
  filter(periodyear == 2010) %>%
  arrange(desc(population)); growers

bridges_county <- subset(bridges, grepl(" County", bridges$Jurisdiction, perl=TRUE))
pop %>%
  select(statename,areatyname,areaname,periodyear,periodtype, population) %>%
  filter(areatyname == "County" & statename == "Virginia" & periodtype ==1) %>%
  group_by(areaname) %>%
  summarise(min_pop = min(population),
            max_pop = max(population),
            avg_pop = round(mean(population)),
            sd_pop = sd(population),
            var_pop = var(population)) %>%
  arrange(desc(max_pop)) 


# How many unique years (time span) do we have?
pop %>%
  summarise(unique_years = n_distinct(periodyear))

# NOTE: even filtering on areatyname==county and areatype, independent cities still get listed
pop %>%
  filter(areatyname=="County" & statename == "Virginia" & areatype ==4) %>%
  group_by(areaname) %>%
  summarise(max_pop = max(population)) %>%
  arrange(desc(max_pop))

areanames = pop %>%
  group_by(areaname) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

###### CES Dataset #######################################################
ces %>%
  filter(areatyname == "State" & statename == "Virginia", period ==0) %>%
  group_by(areaname) 
#  summarise(count = n()) %>%
#  arrange(desc(count))

table(ces$statename)
table(ces$areatyname)
# Number of unique industrial series code for Virginia
summarise(filter(ces,areaname=="Virginia"), unique_ind = n_distinct(seriescode))

############ Industry Dataset #############################################
# To deal with chr type for indcode, in order to deal with multi-level aggregation of indcode, need to convert indcode to int
# tried getting rid of whitespace while doing read.table(), but that didn't work

fairfax = ind2013 %>%
  select(statename,areatyname,areaname,codetitle,indcode,ownertitle,ownership, firms,estab,avgemp,totwage,avgwkwage,taxwage) %>%
  mutate(indcode2 = as.numeric(substr(ind2013$indcode,1,2))) %>%
  filter(statename == "Virginia", areatyname == "County", areaname == "Fairfax County", codetitle != "Total, All Industries", indcode2 < 100) %>%
#  group_by(codetitle) %>%
  arrange(desc(totwage))
#  summarise(count = n())

############ IndustryBySize Dataset #############################################
fairfax_size = indsize2013 %>%
  select(statename,areatyname,areaname,codetitle,indcode,ownertitle,ownership, sizeclass, estab,avgemp,totwage,avgwkwage) %>%
  filter(statename == "Virginia", areatyname == "County", areaname == "Fairfax County", codetitle != "Total, All Industries")

# Get (with key variables) all company sizes in Fairfax County that do Manufacturing
indsize2013 %>%
  select(statename,areatyname,areaname,codetitle,indcode,ownertitle,ownership, sizeclass, estab,avgemp,totwage,avgwkwage) %>%
  filter(statename == "Virginia", areatyname == "County", areaname == "Fairfax County", ownership == 50, indcode == "1013  ")

############ Bridges Dataset #############################################################

bridges$Single.Sign...Single.Unit.Vehicle...Posted.Capacity..in.tons.= NULL
bridges$Dual.Sign...Single.Unit.Vehicle...Posted.Capacity..in.tons. = NULL
bridges$Dual.Sign...Combination.Unit.Vehicle...Posted.Capacity..in.tons. = NULL
bridges$Weight.Posting.Status=NULL
bridges$Responsibility=NULL
bridges$Func.Obslt=NULL
bridges$Struc.Defic=NULL

## NOTE: Should clean up bridges data prior to analysis (particularly Year.Built values < 1800, cleaning up dates etc.,)
# Get a feel for when bridges were built
# Let's get Year.Built to a numeric value and subset without the NAs
bridges$Year.Built.Numeric <- as.numeric(as.character(bridges$Year.Built))
bridges_numeric_built_df <- subset(bridges,bridges$Year.Built.Numeric != "  NA" & bridges$Year.Built.Numeric > 1800)
summary(bridges_numeric_built_df$Year.Built.Numeric)

# Impact of cleaning up year.built variable: 472 observations removed
obs_removed <- nrow(bridges) - nrow(bridges_numeric_built_df); obs_removed

# convert df back to original name:
bridges <- bridges_numeric_built_df

# Need to convert several chr fields to numeric:
bridges$Avg.Daily.Traffic.Numeric <- as.numeric(as.character(bridges$Avg.Daily.Traffic))
bridges$Health.Index.Numeric <- as.numeric(as.character(bridges$Health.Index))
bridges$Suffic.Rating.Numeric <- as.numeric(as.character(bridges$Suffic.Rating))
bridges$Year.Recnst.State.Numeric <- as.numeric(as.character(bridges$Year.Recnst..State))
bridges$Year.Recnst.Fed.Numeric <- as.numeric(as.character(bridges$Year.Recnst..Fed))

# NOTE: IF I ONLY WANT COUNTY JURISDICTION ENTRIES USE THE FOLLOWING regular expression:
bridges_county <- subset(bridges, grepl(" County", bridges$Jurisdiction, perl=TRUE))

########### Age of Bridge Analysis #################################################
# Plot bridge age distribution
ggplot(bridges, aes(x=Year.Built.Numeric)) + 
  geom_histogram(binwidth=5, fill="red", colour="black") +
  labs(x="Year Bridge Built (binwidth = 5 years)", y="Bridge Count", title = "Distribution of Virginia Bridge Building")

boxplot(bridges$Year.Built.Numeric,
        pars=list(boxwex = 0.4),
        ylab = "Year",
        xlab = "All Bridges",
        main = "Box-Plot of Bridges Built by Year")
rug(jitter(bridges$Year.Built.Numeric, amount = 0.2),side=2,col="red")

# From the box plot and summary, we see that 25% of the bridges (5190) were built prior to 1948 (i.e., greater than or equal to 67 years old). We see several outliers (beyond 1.5 * IQR), in fact, there exist 32 bridges built prior to or within 1900. 50% of bridges (10,380) were built prior to 1968 (greater than or equal to 47 years old). The median and mean are nearly equivalent. 
summary(bridges$Year.Built.Numeric)

table(bridges$Year.Recnst.Fed.Numeric)

# Which year had most bridges built
bigbuildyear <- bridges %>%
  group_by(Year.Built.Numeric) %>%
  summarise(bridges.built = n()) %>%
  arrange(desc(bridges.built))

bridges %>%
  + group_by(Road.System) %>%
  + summarise(roads = n())

# What county has the mean oldest bridges
meanage_jur <- bridges %>%
  group_by(Jurisdiction) %>%
  summarise(meanage = mean(Year.Built.Numeric)) %>%
  arrange(meanage)
meanage_jur

bath.bridges <- subset(bridges,Jurisdiction == "Bath County")
summary(bath.bridges$Year.Built.Numeric)
  
  county.bridge.total <- bridges %>%
  group_by(Year.Built.Numeric) %>%
  summarise(bridges = n()) %>%
  arrange(bridges)

ggplot(bridges, aes(x=Jurisdiction)) + 
  geom_histogram(binwidth=5, fill="red", colour="black") +
  labs(x="Jurisdiction", y="Bridge Count", title = "Distribution by County Virginia Bridges")


bridges %>%
  filter(Jurisdiction == "Fairfax County") %>%
  group_by(Year.Built.Numeric) %>%
  summarise(bridges = n()) %>%
  arrange(desc(bridges))

fairfax.bridges <- subset(bridges, Jurisdiction == "Fairfax County")

boxplot(fairfax.bridges$Year.Built.Numeric,
        pars=list(boxwex = 0.4),
        ylab = "Year",
        main = "Box-Plot of Fairfax Bridges Built by Year")
rug(jitter(fairfax.bridges$Year.Built.Numeric, amount = 0.2),side=2,col="red")

# Next two plots compare Fairfax with overall Virginia bridge construction
ggplot(fairfax.bridges, aes(x=Year.Built.Numeric)) + 
  scale_y_continuous(limits=c(0,3500)) +
  xlim(1800,2014) +
  geom_histogram(binwidth=5, fill="red", colour="black") +
  labs(x="Year", y="Bridge Count", title = "Distribution of Fairfax County Bridge Construction")

ggplot(bridges, aes(x=Year.Built.Numeric)) + 
  geom_histogram(binwidth=5, fill="red", colour="black") +
  labs(x="Year Bridge Built (binwidth = 5 years)", y="Bridge Count", title = "Distribution of Virginia Bridge Construction")

#####

summary(fairfax.bridges$Year.Built.Numeric)
county.bridge.count <- bridges %>%
  group_by(Jurisdiction) %>%
  summarise(bridges = n()) %>%
  arrange(desc(bridges))

### Bridges Sufficiency Rating ###############################
bridges_srnot0 <- subset(bridges,Suffic.Rating.Numeric >= 0)
summary(bridges_srnot0$Suffic.Rating.Numeric)

boxplot(bridges_srnot0$Suffic.Rating.Numeric,
        pars=list(boxwex = 0.4),
        ylab = "Sufficiency Rating",
        main = "Virgina Bridges Sufficiency Ratings")
rug(jitter(bridges_srnot0$Suffic.Rating.Numeric),side=2,col="red")

ggplot(bridges_srnot0, aes(x=Suffic.Rating.Numeric)) + 
  geom_histogram(binwidth=2, fill="red", colour="black") +
  geom_vline(xintercept=c(50,80), linetype="dashed",colour="black") +
  labs(x="Sufficiency Rating", y="Bridge Count", title = "Distribution of Virginia Bridge Sufficiency Ratings")

# How many bridges have sufficiency ratings <= 50
bridges %>%
  filter(Suffic.Rating.Numeric <= 50) %>%
  summarise(badbridges = n()) 

# How many bridges have sufficiency ratings < =80
bridges %>%
  filter(Suffic.Rating.Numeric <= 80) %>%
  summarise(badbridges = n())

########## Which county has the most bridges
bridges %>%
  group_by(Jurisdiction) %>%
  summarise(bridges = n()) %>%
  arrange(desc(bridges))

bridges %>%
  group_by(Year.Built.Numeric) %>%
  summarise(bridges = n()) %>%
  arrange(Year.Built.Numeric)

bridges$Last.Inspected.mdy <- mdy(bridges$Last.Inspected)
summary(year(bridges$Last.Inspected.mdy))


#### Correlation Analysis #######
# create a df that has only numeric covariates and removes those with suffic.rating.number < 0
bridges_redux <- subset(bridges_srnot0, select = c(Year.Built.Numeric, Avg.Daily.Traffic.Numeric, Health.Index.Numeric, Suffic.Rating.Numeric, Year.Recnst.State.Numeric) )
cor(bridges_redux, use="complete.obs")

# The small negative relationship between daily traffic and sufficiency rating is very interesting. What that says is that for higher daily traffic there are lower sufficiency ratings. that is not good. So let's generate a plot...

ggplot(bridges_redux, aes(x=Suffic.Rating.Numeric, y=Avg.Daily.Traffic.Numeric)) + 
  geom_point() +
  geom_hline(yintercept=8109, linetype="dashed",colour="red") +
  geom_vline(xintercept=c(50,80), linetype="dashed",colour="red") +
  labs(x="Bridge Sufficiency Rating", y="Average Daily Traffic", title = "Daily Traffic vs Bridge Sufficiency")

ggplot(bridges_redux, aes(x=Year.Built.Numeric, y=Suffic.Rating.Numeric)) + 
  geom_point() +
  labs(x="Year Built", y="Sufficiency Rating", title = "Bridge Built vs Sufficiency Rating")

bad_bridges <- subset(bridges, Avg.Daily.Traffic.Numeric > 100000 & Suffic.Rating.Numeric < 80)
bad_bridges %>%
  group_by(Jurisdiction) %>%
  summarise(Bad_Bridges = n()) %>%
  arrange(desc(Bad_Bridges))

more_bad_bridges <- subset(bridges, Year.Built.Numeric > 2000 & Suffic.Rating.Numeric < 80)
more_bad_bridges %>%
  group_by(Jurisdiction) %>%
  summarise(Bad_Bridges = n()) %>%
  arrange(desc(Bad_Bridges)) %>%
  filter(Bad_Bridges > 4)


# Let's create a Poisson Model with Bridge Sufficiency Rating as response variable

suff.mod1 <- lm(Suffic.Rating.Numeric ~ Avg.Daily.Traffic.Numeric + Year.Built.Numeric + factor(Jurisdiction), data=bridges)
summary(suff.mod1)
confint(suff.mod1)
exp(coef(suff.mod1))


############# Income Dataset ######################################################
glimpse(inc)
county_inc_pop = inc %>%
  select(statename,areatyname,areaname,periodyear,income, population) %>%
  filter(areatyname == "County" & statename == "Virginia" & (periodyear == 2006 | periodyear == 2007 | periodyear == 2008 | periodyear == 2009 | periodyear == 2009 | periodyear == 2010 | periodyear == 2011))

write.csv(county_inc_pop, file = "county_inc_pop.csv", row.names=FALSE)

############ Labor Force Dataset ##################################################
glimpse(labor)
county_labor = labor %>%
  select(statename, areatyname, areaname, periodyear,pertypdesc, laborforce, unemp) %>%
  filter(areatyname=="County" & statename=="Virginia" & pertypdesc=="Annual" & (periodyear==2006 | periodyear==2007 | periodyear==2008 | periodyear==2009 | periodyear==2010 | periodyear==2011))

write.csv(county_labor,file="county_labor.csv", row.names=FALSE)

############ Suicide Dataset ####################################################
# NoVA counties include: Fairfax, Frederick, Clarke, Loudoun, Shenandoah, Warren Rappahannock, Fauquier, Prince William, Rockingham, Greene, Page, Culpepper, Stafford, Madison

# Look at response variable:
suicide.count <- table(suicide$suicides)
dataf.suicide.count = data.frame(prop.table(table(suicide$suicides)))
dataf.suicide.count$cumulative = cumsum(dataf.suicide.count$Freq)
dataf.suicide.all = data.frame(suicide.count,dataf.suicide.count$Freq*100, dataf.suicide.count$cumulative * 100)
dataf.suicide.all
head(dataf.suicide.all)

###  Let's create some additional variables and dataframes:
suicide$divorce.percent <- suicide$divorces/suicide$pop_labor * 100   # Divorce rate
suicide$unemp.percent <- suicide$pop_unemp/suicide$pop_labor * 100  # Unemployment rate
suicide_nofax <- subset(suicide, county!="fairfax")

# Mean and variance of response variable
suic.mean <- mean(suicide$suicides); suic.mean
suic.var <- var(suicide$suicides); suic.var

# Note: by excluding Fairfax, the variance is reduced by an order of magnitude
suic.mean.nofax <- mean(suicide_nofax$suicides); suic.mean.nofax
suic.var.nofax <- var(suicide_nofax$suicides); suic.var.nofax

# Assume a Poisson distribution, what would be the percentage/probability of zero suicides:
(exp(-suic.mean) * suic.mean^0)/factorial(0) * nrow(suicide)  
(exp(-suic.mean.nofax) * suic.mean.nofax^0)/factorial(0) * nrow(suicide_nofax) 

# We expect less than 1% of 0-count suicides versus the observed value of 6.3%
# Thus a Poisson model would be overdispersed. We also see that by comparing the population mean and variance (should be nearly equal)

# From the plot below, we see there are about 6 outliers, suicides > 80
suicide %>%
  ggvis(~suicides, fill := "red") %>%
  layer_histograms(width = 1)

ggplot(suicide, aes(x=suicides)) +
  geom_histogram(binwidth=2, fill="red", colour="black") + 
  labs(x = "Suicides (binwidth = 1)", y = "County County", title = "Distribution of Virginia Suicides") + 
  scale_x_continuous(limits=c(0,110))

boxplot(suicide$suicides,
        pars=list(boxwex = 0.4),
        ylab = "Number of Suicides",
        xlab = "All Counties",
        main = "Box-Plot of Suicides by County Each Year (2006-2011)")
rug(jitter(suicide$suicides, amount = 0.2),side=2,col="red")

ggplot(suicide, aes(x = year, y=suicides)) + geom_boxplot()

# Do outliers belong to a particular county? Yes:
subset(suicide, suicides > 80)     # It's Fairfax

suicide %>%
  select(unemp.percent > 0.03) %>%
  ggvis(~unemp.percent, ~suicides.poptot, fill:="red") %>%
  layer_points()

p = ggplot(suicide,aes(unemp.percent > 0.03,suicides.poptot,colour=factor(county))) + geom_point()
p

# Let's look at some of the predictors
# Counties:
plot(suicide$pop_tot, ylab = "populations", xlab="observations", main="6 Year Groupings of County Populations")

ggplot(suicide, aes(x=pop_tot)) + 
  geom_histogram(binwidth=20000, fill="red", colour="black") +
  labs(x="Total Population", y="County Count", title = "Distribution of County Populations")

ggplot(suicide_nofax, aes(x=pop_tot)) + 
  geom_histogram(binwidth=5000, fill="red", colour="black") +
  xlab("Total Population") + ylab("County Count") + 
  title("County Population Distribution")

suicide %>%
  select(county, year, pop_tot) %>%
  filter(pop_tot > 150000 & year==2011) %>%
  arrange(desc(pop_tot,desc))

suicide %>%
  ggvis(~divorces, fill:="red") %>%
  layer_histograms(width = 10)

ggplot(suicide, aes(x=divorces)) + 
  geom_histogram(binwidth=20, fill="red", colour="black")

suicide %>%
  ggvis(~divorce.percent, fill:="red") %>%
  layer_histograms(width = 0.1)

suicide %>%
  ggvis(~unemp.percent, fill:="red") %>%
  layer_histograms(width = 0.5)

suicide %>%
  ggvis(~income_med_house, fill:="red") %>%
  layer_histograms(width = 1000)

suicide %>%
  ggvis(~pop_tot, fill:="red") %>%
  layer_histograms(width = 10000)

# Let's look at some potential relationships:
suicide %>%
  ggvis(~pop_tot, ~ suicides, fill:= "red") %>% 
  layer_points() %>%
  layer_smooths()

suicide %>%
  ggvis(~suicides, ~ divorces, fill:= "red") %>% 
  layer_points() %>%
  layer_smooths()

suicide %>%
  ggvis(~percent_unemp, ~ percent_divorces, fill:= "red") %>% 
  layer_points() %>%
  layer_smooths()

## NOTE: We should include an interaction between unemployment and divorce in statistical model

suicide %>%
  ggvis(~pop_unemp, ~suicides, fill:= "red") %>% 
  layer_points() %>%
  layer_smooths()

suicide %>%
  ggvis(~pop_tot, ~ suicides.poptot, fill:= "red") %>% 
  layer_points() %>%
  layer_smooths()

# Surprising result here, as divorce.percent goes up, suicides go down (slightly)
suicide_nofax %>%
  ggvis(~divorce.percent, ~suicides, fill:= "red") %>% 
  layer_points() %>%
  layer_model_predictions(model = "lm")

ggplot(suicide, aes(x=divorce.percent, y=suicides), colour="red") + 
  geom_point(colour="red", size=3) +
  labs(x="Percent of Divorced Labor Population", y="Suicides", title = "Suicides vs Divorce Percentage")

suicide %>%
  filter(pop_tot < 150000) %>%
  filter(suicides < 20) %>%
  ggvis(~percent_unemp, ~ suicides, fill:= "red") %>% 
  layer_points() %>%
  layer_smooths()

subset(suicide, pop_tot > 190000)

# Develop the Poisson model
suicmod1 = glm(suicides ~ income_med_house + divorces + pop_unemp + pop_tot + factor(county), family="poisson", offset=log(pop_tot), data=suicide)
summary(suicmod1)
pearson.disp <- sum(residuals(suicmod1, type="pearson")^2); pearson.disp
total.disp <- pearson.disp/df.residual(suicmod1); total.disp   # total.disp is low for suic model
exp_df <- exp(coefficients(suicmod1)); exp_df
presid <- residuals(suicmod1, type="pearson")
respon <- residuals(suicmod1,type="response")
P__disp(suicmod1)
mu <- predict(suicmod1)
par(mfrow=c(1,2))

# The Deviance GOF test is based on the view that the deviance is distributed as Chi2. Deviance is in effect a measure of the distance between the most full (saturated) model we can fit, and the proposed model we are testing for fit. Or more succinctly, the difference between a saturate log-likelihood and the log-likelihood full model:


# If the resulting Chi2 p-value is less than 0.05, the model is considered well fit. Since the Chi2 p-value is less than 0.05, our model is a good fit
# This test statistic evaluates whether the value of the deviance, for a specific model size, is close enough to that of the saturated model that it cannot be rejected as fitting

dev = deviance(suicmod1)
df = df.residual(suicmod1)
p_value = 1 - pchisq(dev,df)
p_value

# Since we found the following 4 counties are significant, let's look at them
subset(suicide, (county=='dickenson' | county=='carroll' | county=='buchanan' | county=='henry') & year==2011, select=c(county,year,pop_tot,pop_labor,divorce.percent,unemp.percent))


ggplot(suicide, aes(x=mu, y=respon)) + 
  geom_point(shape=1, colour="red") +
  labs(x="Predicted value (mu)", y="Response Residual", title = "Response Residual Plot")

foo <- predict(suicmod1,type="response")
errors <- suicide$suicides - foo

ggplot(suicide, aes(x=foo, y=errors)) + 
  geom_point(shape=1, colour="red") +
  labs(x="Predicted value (mu)", y="Response Residual", title = "Response Residual Plot")

ggplot(suicide, aes(x=mu, y=presid)) + 
  geom_point(shape=1, colour="red") +
  labs(x="Predicted value (mu)", y="Pearson Residual", title = "Pearson Residual Plot")

suicmod2 = glm(suicides ~ pop_unemp + divorces + income_med_house, family="poisson", offset=log(pop_tot), data=suicide)
summary(suicmod2)
pearson.disp <- sum(residuals(suicmod2, type="pearson")^2); pearson.disp
total.disp <- pearson.disp/df.residual(suicmod2); total.disp   # total.disp is low for suic model

# Deviance GOF:
dev = deviance(suicmod2)
df = df.residual(suicmod2)
p_value = 1 - pchisq(dev,df)
p_value
presid <- residuals(suicmod2, type="pearson")
respon <- residuals(suicmod2,type="response")
P__disp(suicmod2)
mu <- predict(suicmod2)
par(mfrow=c(1,2))
plot(x=mu,y=respon, main="Response residuals")
plot(x=mu, y=presid, main="Pearson residuals")

drop1(suicmod2, test="Chisq")

suicmod3 = glm(suicides ~ pop_unemp + pop_tot + divorces + income_med_house, family="poisson", offset=log(pop_tot), data=suicide_nofax)
summary(suicmod3)
pearson.disp <- sum(residuals(suicmod3, type="pearson")^2); pearson.disp
total.disp <- pearson.disp/df.residual(suicmod3); total.disp   # total.disp is low for suic model
confint(suicmod3)
exp(coef(suicmod3))

# Deviance GOF:
dev = deviance(suicmod3)
df = df.residual(suicmod3)
p_value = 1 - pchisq(dev,df)
p_value


suicide_nofax <- subset(suicide, county!="fairfax")
suicmod2 = glm(suicides ~ income_med_house + divorces + pop_unemp + factor(county) + pop_tot, family="poisson", offset=log(pop_tot), data=suicide_nofax)
summary(suicmod2)


pearson.disp2 <- sum(residuals(suicmod2, type="pearson")^2); pearson.disp2 
total.disp2 <- pearson.disp2/df.residual(suicmod2); total.disp2 

presid2 <- residuals(suicmod2, type="pearson")
respon2 <- residuals(suicmod2,type="response")


mu2 <- predict(suicmod2, type="response")  # calculate the predicted value on scale of response variable


ggplot(suicide_nofax, aes(x=mu2, y=respon2)) + 
  geom_point(shape=1, colour="red") +
  labs(x="Predicted value (mu)", y="Raw Residual", title = "Raw Residual Plot sans Fairfax")


suicmod3 = glm(suicides ~ income_med_house + divorces + pop_unemp + pop_tot, family="poisson", offset=log(pop_tot), data=suicide_nofax)
summary(suicmod3)
pearson.disp3 <- sum(residuals(suicmod3, type="pearson")^2); pearson.disp3
total.disp3 <- pearson.disp3/df.residual(suicmod3); total.disp3   # total.disp is low for suic model
exp_df3 <- exp(coefficients(suicmod3)); exp_df3
presid3 <- residuals(suicmod3, type="pearson")
respon3 <- residuals(suicmod3,type="response")
P__disp(suicmod3)
mu3 <- predict(suicmod3)
par(mfrow=c(1,2))

# The Deviance GOF test is based on the view that the deviance is distributed as Chi2. Deviance is in effect a measure of the distance between the most full (saturated) model we can fit, and the proposed model we are testing for fit. Or more succinctly, the difference between a saturate log-likelihood and the log-likelihood full model:


# If the resulting Chi2 p-value is less than 0.05, the model is considered well fit. Since the Chi2 p-value is less than 0.05, our model is a good fit
# This test statistic evaluates whether the value of the deviance, for a specific model size, is close enough to that of the saturated model that it cannot be rejected as fitting

dev3 = deviance(suicmod3)
df3 = df.residual(suicmod3)
p_value3 = 1 - pchisq(dev3,df3)
p_value3



##### Negative Binomial Model ##################
# How does the NB Model differ from the Poisson Model. The mean is understood in the same manner as the Poisson mean, but the variance has a much wider scope than is allowed by the Poisson distribution. The negative binomial is a tw-parameter model, with mean mu and dispersion alpha parameters. The important takeaway is that the negative binomial (NB) allows us to model a greater variance in the data than the Poisson. Consequently, the NB model is most always used to estimate the parameters of overdispersed data. 

# R users must be careful in how the understand the glm.nb dispersion parameters since it is the inverse of alpha
# as a result, better to use the nbinomial function in the COUNT or msme packages. 
suicmod4 <- glm.nb(suicides ~ income_med_house + divorces + pop_unemp + pop_tot, data=suicide, control=glm.control(maxit=100))

summary(suicmod4)


