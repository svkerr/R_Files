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

############ Bridges Dataset ####################################################
bridges$Single.Sign...Single.Unit.Vehicle...Posted.Capacity..in.tons.= NULL
bridges$Dual.Sign...Single.Unit.Vehicle...Posted.Capacity..in.tons. = NULL
bridges$Dual.Sign...Combination.Unit.Vehicle...Posted.Capacity..in.tons. = NULL
bridges$Weight.Posting.Status=NULL
bridges$Responsibility=NULL
bridges$Func.Obslt=NULL
bridges$Struc.Defic=NULL

# Get a feel for when bridges were built
county_bridges = bridges %>%
  group_by(Year.Built) %>%
  summarise(bridges = n()) %>%
  arrange(desc(bridges))

bridges %>%
  filter(Jurisdiction == "Fairfax County") %>%
  group_by(Year.Built) %>%
  summarise(bridges = n()) %>%
  arrange(desc(bridges))

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

############ Suicide Dataset ############################################
# NoVA counties include: Fairfax, Frederick, Clarke, Loudoun, Shenandoah, Warren Rappahannock, Fauquier, Prince William, Rockingham, Greene, Page, Culpepper, Stafford, Madison
table(suicides$suicides)
suic = glm(suicides ~ income_med_house + divorces + factor(county), family="poisson", offset=log(pop_tot),data=suicide)
summary(suic)
pearson.disp <- sum(residuals(suic, type="pearson")^2); pearson.disp
total.disp <- pearson.disp/df.residual(suic); total.disp   # total.disp is low for suic model
table.suicide <-table(suicide$suicides); table.suicide
mean.suicide <- mean(suicide$suicides); mean.suicide
var.suicide <- var(suicide$suicides); var.suicide
# NOTE: response mean is nowhere near the variance (Poisson questionable?)
