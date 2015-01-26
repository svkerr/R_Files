# Project: NOVA
# Analyzing Virginia data

# Set working directory where data files reside
setwd("/Users/stuart/DataSets/virginia/")

# Load key libraries
library(dplyr)
library(ggvis)
library(ggplot2)
library(lubridate)

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
va_commwork %>%
  group_by(statename) %>%
  summarise(count = n(), num_workers = sum(workers)) %>%
  arrange(desc(num_workers))

###### Population Dataset ##########################################
# Create some dataframe subsets to analyze
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
ggvis((max_pop))

# How many unique years (time span) do we have?
pop %>%
  summarise(unique_years = n_distinct(periodyear))

# NOTE: even filtering on areatyname==county and areatype, independent cities still get listed
pop %>%
  filter(areatyname=="County" & statename == "Virginia" & areatype ==4) %>%
  group_by(areaname) %>%
  summarise(max_pop = max(population)) 
#  summarise(num_counties = n())

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
county_bridges = bridges %>%
  group_by(Year.Built) %>%
  summarise(bridges = n()) %>%
  arrange(desc(bridges))

bridges %>%
  filter(Jurisdiction == "Fairfax County") %>%
  group_by(Year.Built.Numeric) %>%
  summarise(bridges = n()) %>%
  arrange(desc(bridges))

bridges %>%
  group_by(Year.Built.Numeric) %>%
  summarise(bridges = n()) %>%
  arrange(Year.Built.Numeric)

# Let's get Year.Built to a numeric value and subset without the NAs
bridges$Year.Built.Numeric <- as.numeric(as.character(bridges$Year.Built))
bridges_numeric_built <- subset(bridges,bridges$Year.Built.Numeric != "  NA")
summary(bridges_numeric_built$Year.Built.Numeric)
str(bridges_numeric_built)

bridges$Last.Inspected.mdy <- mdy(bridges$Last.Inspected)
summary(year(bridges$Last.Inspected.mdy))

# Percentage of bridges eliminated by cleaning up non-numeric year.built data entries
(nrow(bridges) - nrow(bridges_numeric_built))/nrow(bridges) * 100

bridges %>%
  filter(Year.Built.Numeric > 1800) %>%
  ggvis(~Year.Built.Numeric) %>%
  layer_histograms(width=1)

# Using ggplot mainly to be able to export plot to an image (ggvis does not allow this)
bridges %>%
  filter(Year.Built.Numeric > 1800) %>%
  ggplot(aes(x=Year.Built.Numeric)) + geom_histogram(binwidth=2,fill="steelblue") + ggtitle("Virginia Bridges (All Types)")


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

ggplot(suicide, aes(x=county,y=suicides)) + geom_boxplot()

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





##### Negative Binomial Model ##################
# How does the NB Model differ from the Poisson Model. The mean is understood in the same manner as the Poisson mean, but the variance has a much wider scope than is allowed by the Poisson distribution. The negative binomial is a tw-parameter model, with mean mu and dispersion alpha parameters. The important takeaway is that the negative binomial (NB) allows us to model a greater variance in the data than the Poisson. Consequently, the NB model is most always used to estimate the parameters of overdispersed data. 

# R users must be careful in how the understand the glm.nb dispersion parameters since it is the inverse of alpha
# as a result, better to use the nbinomial function in the COUNT or msme packages. 
suicmod4 <- glm.nb(suicides ~ income_med_house + divorces + pop_unemp + pop_tot, data=suicide, control=glm.control(maxit=100))

summary(suicmod4)


