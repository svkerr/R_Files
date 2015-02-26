# Scripts that experiment with dply package

setwd("/Users/stuart/R_Files")

# NOTE: if using dplyr, do not load plyr (known to mess with at least "summarise" )
library(dplyr)
library(ggplot2)
library(ggvis)
library(Lahman)  # for baseball data

options(digits = 6)

# Read in data files
flights_2007 = read.table("/Users/stuart/DataSets/Airlines/2007.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
flt2007 = tbl_df(flights_2007)
print(object.size(flt2007), units= "GB")

### Or using dplyr  flights data #######################################
library(hflights)
data(hflights)
head(hflights)

# convert data to a "dplyr data frame"
flights = tbl_df(hflights)
flights   # kinda don't need to use head(flights)
str(flights)
glimpse(flights)

# Use unix pipe | for an 'OR' condition
filter(flights,UniqueCarrier=='AA' | UniqueCarrier=='UA')

# Find all flights to SFO or OAK, in January, delayed by more than an hour, that departed between midnight and 5am, where the arrival delay was twice the departure delay

flights %>%
  filter(Dest == 'OAK' | Dest=='SFO') %>%
  filter(Month == 1) %>%
  filter(DepDelay > 60) %>%
  filter(DepTime < 1200) %>%
  filter(ArrDelay > 2 * DepDelay)

# Order flights by departure date:
flights %>%
  arrange(DayofMonth,DepTime)

# Pick columns by name versus awkward R approach
select(flights, DepTime,ArrTime,FlightNum)

# Use pipelining operator %>% to avoid R nested statements
# And use "arrange" to reorder rows
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60) %>%
  arrange(desc(DepDelay))

# Use "mutate" to create new variables that are functions of existing variables
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)

# To add new variable to dataframe
flights <- flights %>% mutate(Speed = Distance/AirTime*60)
str(flights)

# Find fastest airlines
flights %>%
  select(UniqueCarrier, TailNum, Origin, Dest, Speed) %>%
  arrange(desc(Speed))

# Isolate that really fast flight
> flights %>%
  filter(TailNum=='N11612' & Speed > 600) %>%
  data.frame()

# Use "summarise": Primarily useful with data that has been grouped by one or more variables
# "group_by": creates the groups that will be operated on
# "summarise": uses the provided aggregation function to summarise each group
# Create a table grouped by Dest, then summarise each group by taking the mean of ArrDelay
flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE)) %>%
  arrange(desc(avg_delay))

# "summarise_each" allows you to apply the same summary function to multiple columns at once
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean), Cancelled, Diverted)

# But be careful, if a column contains NAs, then the na.rm = T needs to be embedded with funs functions
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean(.,na.rm=TRUE)), Cancelled, Diverted, AirTime)

# To embellish, arrange by descending AirTime
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean(.,na.rm=TRUE)), Cancelled, Diverted, AirTime) %>%
  arrange(desc(AirTime))

# for each carrier, calculate the minimum and maximum arrival and departure delays
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)), matches("Delay"))

# Helper function n() counts the number of rows in a group
# Helper function n_distinct(vector) counts the number of unique items in the vector
flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

# Or use the tally() function:
flights %>%
  group_by(Month,DayofMonth) %>%
  tally(sort = T)

# for each destination, count the total number of flights and the number of distinct planes that flew there
flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))

# Now using dplyr 0.3 with new functions: (distinct, count,) ###########
# Find all origin-destination pairs
flights %>% 
  select(Origin, Dest) %>%
  distinct()

# How many flights to each destination?
flights %>% 
  count(Dest, sort=TRUE) 

# Which planes flew the most
flights %>% 
  count(TailNum, sort=TRUE)

# Let's do some graphing ########################
flights %>%
  ggvis(~Distance) %>%
  layer_histograms()

flights %>%
  ggvis(~Month) %>%
  layer_histograms()

flights %>%
  ggvis(~UniqueCarrier)

# Let's do some modeling
model1 = lm(hflights$DepDelay ~ hflights$AirTime)
confint(model1)

# Test of the obvious
model2 = lm(hflights$AirTime ~ hflights$Distance)
summary(model2)
confint(model2)

model3 = lm(hflights$DepDelay ~ hflights$Distance)
summary(model3)
confint(model3)

plot(hflights$Distance)
# Plots that will explain low R-Squared and low p-value
plot(hflights$Distance,hflights$DepDelay)
plot(hflights$Distance,hflights$AirTime)

# Define a data set named d that contains just the Dest, UniqueCarrier, Distance, and ActualElapsedTime columns of hflights as well as two additional variables: RealTime and mph.RealTime should equal the actual elapsed time plus 100 minutes. This will be an estimate of how much time a person spends getting from point A to point B while flying, including getting to the airport, security checks, etc. mph will be the miles per hour that a person on the flight traveled based on the RealTime of the flight.

#On many highways you can drive at 70 mph. Continue with d to calculate the following variables: n_less, the number of flights whose mph value does not equal NA that traveled at less than 70 mph in real time; n_dest, the number of destinations that were traveled to at less than 70 mph; min_dist, the minimum distance of these flights; max_dist, the maximum distance of these flights.

d = flights %>%
  select(Dest,UniqueCarrier,Distance,ActualElapsedTime) %>%
  mutate(RealTime = ActualElapsedTime + 100, 
         mph = Distance/RealTime * 60)

e = d %>%
  filter(!is.na(mph) & mph <= 70) %>%
  summarise(n_less = n(),
            n_dest = n_distinct(Dest), 
            min_dist = min(Distance), 
            max_dist = max(Distance))

# Let's define preferable flights as flights that are 150% faster than driving, i.e. that travel 105 mph or greater in real time. Also, assume that cancelled or diverted flights are less preferable than driving.
# Write an adapted version of the solution to the previous exercise in an all-in-one fashion (i.e. in a single expression without intermediary variables, using %>%) to find:

# n_non - the number of non-preferable flights in hflights,
# p_non - the percentage of non-preferable flights in hflights,
# n_dest - the number of destinations flights that non-preferable flights traveled to,
# min_dist - the minimum distance that non-preferable flights traveled,
# max_dist - the maximum distance that non-preferable flights traveled.

# To maintain readability in this advanced exercise, start your operations with a select() function to retain only the five columns that will be needed for the subsequent calculation steps.

f = flights %>%
  select(Dest,Cancelled, Diverted, Distance,ActualElapsedTime) %>%
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance/RealTime * 60) %>%
  filter(mph <= 105 | Cancelled ==1 | Diverted == 1) %>%
  summarise(n_non = n(),
            p_non = n_non/nrow(flights) * 100,
            n_dest = n_distinct(Dest),
            min_dist = min(Distance),
            max_dist = max(Distance))
#How many flights were overnight flights (flights whose arrival time is earlier than their departed time). Do not count NA values!

flights %>%
  filter(!is.na(ArrTime) & !is.na(DepTime)) %>%
  summarise( overnight = n())

# Demonstrate power of combining group_by with summarise ######
flights %>%
  group_by(UniqueCarrier) %>%
  summarise(avgDep = mean(DepDelay, na.rm = T),
            avgArr = mean(ArrDelay, na.rm = T)) %>%
  arrange(desc(avgArr, avgDep))

# Use group_by() and summarise() to compare the individual carriers. For each carrier, count the total number of flights flown by the carrier (n_flights), the total number of cancelled flights (n_canc), the percentage of cancelled flights (p_canc), and the average arrival delay of the flights whose delay does not equal NA (avg_delay). Once you've calculated these results, arrange() the carriers from low to high by their average arrival delay. Use percentage of flights cancelled to break any ties. Which airline scores best based on these statistics?

flights %>%
  group_by(UniqueCarrier) %>%
  summarise(n_flights = n(), 
            n_canc = sum(Cancelled ==1),
            p_canc = n_canc/n_flights * 100,
            p2_canc = mean(Cancelled ==1) * 100,
            avg_delay = mean(ArrDelay, na.rm = T)) %>%
  arrange(desc(avg_delay, p_canc))

# Come up with a way to answer this question: At which day of the week is the average total taxiing time highest? Use group_by(), summarise() and arrange(); you should avoid the use of mutate(). Define the grouped average total taxiing time to be avg_taxi.

flights %>%
  group_by(DayOfWeek) %>%
  summarise( avg_taxi = mean(TaxiIn + TaxiOut, na.rm = T) )

# Discard flights whose arrival delay equals NA, then rank the carriers by the proportion of their flights that arrived delayed (call this variable p_delay) and arrange the results based on the ranking. Create the rank variable explicitly using mutate().
flights %>%
  group_by(UniqueCarrier) %>%
  filter(!is.na(ArrDelay)) %>%
  summarise(p_delay = mean(ArrDelay > 0)) %>%
  mutate(rank = rank(p_delay)) %>%
  arrange(rank)

# In a similar fashion, rank the carriers by the average delay of flights that are delayed (ArrDelay > 0). Then arrange the data based on the results. Again, build a rank variable explicitly.

flights %>%
  group_by(UniqueCarrier) %>%
  filter(!is.na(ArrDelay) & ArrDelay > 0) %>%
  summarise(avg = mean(ArrDelay)) %>%
  mutate(rank = rank(avg)) %>%
  arrange(rank)

# Which plane (by tail number) flew out of Houston the most times? How many times? Assign the result to adv1.To answer this question precisely, you will have to filter() as a final step to end up with only two numbers in your dataset.
flights %>%
  group_by(TailNum) %>%
  summarise(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  head(n = 1)

# or this approach
flights %>%
  group_by(TailNum) %>%
  summarise(num_flights = n()) %>%
  select(TailNum,num_flights) %>%
  filter(num_flights == max(num_flights))

# How many airplanes only flew to one destination from Houston? Save the resulting dataset in adv2
flights %>%
  group_by(TailNum) %>%
  summarise(n_dest = n_distinct(Dest)) %>%
  filter(n_dest == 1) %>%
  summarise(ones = n())

# Find the most visited destination for each carrier and save your solution to adv3.
flights %>%
  group_by(UniqueCarrier, Dest) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n)) ) %>%
  filter(rank == 1.0)

# For each destination, find the carrier that travels to the destination the most. Store the result in adv4. NOte: to change it to where for each destination, to find the carrier that travels least to the destination, change: rank = rank(desc(n)) to rank = rank(n)
flights %>%
  group_by(Dest, UniqueCarrier) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)

# An attempt to use the set function intersect()

set1 <- flights %>%
  filter(DayofMonth == 31 & Dest == "DFW" & Distance > 25) %>%
  select(TailNum)

set2 <- flights %>%
  filter(DayofMonth == 1 & Dest == "DFW" & Distance > 25) %>%
  select(TailNum)

intersect(set1,set2)

# Use summarise() to calculate n_carrier, the total number of unique carriers in hflights2. Save the result to the variable s2. Whether or not you use the pipe is up to you!

library(data.table)
hflights2 = as.data.table(hflights)

hflights2 %>%
  summarise(n_carrier = n_distinct(UniqueCarrier))

##### Baseball dataset example ####################
# Uses Lahman library
big_df = merge(Batting, Master, by="playerID")
big_df = tbl_df(big_df)

# At what age do MLB players reach their home run hitting peak?
# This excludes those that had 0 homeruns
big_df %>%
  select(playerID, yearID, birthYear, HR) %>%
  mutate(age = yearID - birthYear) %>%
  filter(!is.na(HR) & !is.na(age)) %>%
  group_by(playerID) %>%
  mutate(rank = rank(desc(HR))) %>%
  filter(rank == 1) %>%
  ggvis(~age) %>%
  layer_histograms()
  

