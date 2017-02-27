# load packages
library(dplyr)
#install.packages("hflights")
library(hflights)

# explore data
data(hflights)
head(hflights)

# convert to local data frame
flights <- tbl_df(hflights)

# printing only shows 10 rows and as many columns as can fit on your screen
flights


# you can specify that you want to see more rows
print(flights, n=20)

# convert to a normal data frame to see all of the columns
data.frame(head(flights))

### FILTER

# base R approach to view all flights on January 1
flights[flights$Month==1 & flights$DayofMonth==1,]

# dplyr approach
# note: you can use comma or ampersand to represent AND condition
filter(flights, Month == 1, DayofMonth == 1)

# you can also use %in% operator
filter(flights, UniqueCarrier %in% c("AA", "UA"))


### SELECT

#base R approach to select DepTime, ArrTime, and FlightNum columns
flights[, c("DepTime", "ArrTime", "FlightNum")]

# dplyr approach
select(flights,  DepTime, ArrTime, FlightNum)


# use colon to select multiple contiguous columns, and use `contains` to match columns by name
# note: `starts_with`, `ends_with`, and `matches` (for regular expressions) can also be used to match columns by name
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))


###CHAINING

# nesting method to select UniqueCarrier and DepDelay columns and filter for delays over 60 minutes
filter(select(flights, UniqueCarrier,DepDelay), DepDelay>60)

# chaining method
flights %>% 
  select(UniqueCarrier,DepDelay) %>%
  filter(DepDelay>60)


# create two vectors and calculate Euclidian distance between them
x <- 3:8;y <- 5:10; 
sqrt(sum((x-y)^2))
(x-y)^2 %>% sum() %>% sqrt()


###ARRANGE

# base R approach to select UniqueCarrier and DepDelay columns and sort by DepDelay
flights[order(flights$DepDelay, decreasing = T), c("UniqueCarrier", "DepDelay")]

# dplyr approach
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay))
  
  

###MUTATE: Add new variables

# base R approach to create a new variable Speed (in mph)
flights$SpeedBase <- flights$Distance/flights$AirTime*60
flights[,c("Distance","SpeedBase", "AirTime")]



# dplyr approach (prints the new variable but does not store it)
flights %>%
  select(Distance, AirTime, UniqueCarrier) %>%
  mutate(Speed = Distance/AirTime*60)


#SUMMARISE - group_by

# base R approaches to calculate the average arrival delay to each destination
with(flights, tapply(ArrDelay, Dest, mean, na.rm=T, simplify = F))
?tapply
#?aggregate
aggregate(ArrDelay~Dest, data = flights, mean)
aggregate(cbind(ArrDelay,DepDelay)~Dest, data = flights, mean)

# dplyr approach: create a table grouped by Dest, 
 #and then summarise each group by taking the mean of ArrDelay
flights%>%
  group_by(Dest)%>%
  summarise(AvgDelay  = mean(ArrDelay, na.rm=T))

# for each carrier, calculate the percentage of flights cancelled or diverted
aggregate(cbind(Cancelled,Diverted,Run= !Cancelled&!Diverted)~UniqueCarrier, 
          data = flights, sum)
names(flights)
summary(flights)

flights%>%
  group_by(UniqueCarrier)%>%
  summarise(CancelledDiverted = sum(Cancelled, na.rm=T)+sum(Diverted, na.rm=T))

#summarise_each
flights%>%
  group_by(UniqueCarrier)%>%
  summarise_each(funs(mean),Cancelled, Diverted )

#----------------------------------------------------------------------------
?summarise_all
by_species <- iris %>% group_by(Species)
summary(by_species)
by_species %>% summarise_all(n_distinct)
by_species %>% summarise_all(mean)

# Use the _at and _if variants for conditional mapping.
by_species %>% summarise_if(is.numeric, mean)


# summarise_at() can use select() helpers with the vars() function:
by_species %>% summarise_at(vars(Petal.Width), mean)
by_species %>% summarise_at(vars(matches("Width")), mean)


# You can also specify columns with column names or column positions:
by_species %>% summarise_at(c("Sepal.Width", "Petal.Width"), mean)
by_species %>% summarise_at(c(1, 3), mean)


# You can provide additional arguments. Those are evaluated only once:
by_species %>% summarise_all(mean, trim = 1)
by_species %>% summarise_at(vars(Petal.Width), mean, trim = 1)


# You can provide an expression or multiple functions with the funs() helper.
by_species %>% mutate_all(funs(. * 0.4))
by_species %>% summarise_all(funs(min, max))
# Note that output variable name must now include function name, in order to
# keep things distinct.


# Function names will be included if .funs has names or whenever multiple
# functions are used.
by_species %>% mutate_all(funs("in" = . / 2.54))
by_species %>% mutate_all(funs(rg = diff(range(.))))
by_species %>% summarise_all(funs(med = median))
by_species %>% summarise_all(funs(Q3 = quantile), probs = 0.75)
by_species %>% summarise_all(c("min", "max"))


# Two functions, continued
by_species %>% summarise_at(vars(Petal.Width, Sepal.Width), funs(min, max))
by_species %>% summarise_at(vars(matches("Width")), funs(min, max))
#--------------------------------------------------------------------------------------------

#CONTINUED...
# for each day of the year, count the total number of flights and sort in descending order
flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>% #n() - counts the number of rows in a group
  arrange(desc(flight_count))

# rewrite more simply with the `tally` function
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)

# for each destination, count the total number of flights and the number of 
  #distinct planes that flew there
flights%>%
  group_by(Dest)%>%
  summarise(flightCount = n(), PlaneCount = n_distinct(TailNum))%>%
  arrange(desc(PlaneCount)) #n_distinct(vector) - counts the number of unique items in that vector


# for each destination, show the number of cancelled and not cancelled flights
flights%>%
  group_by(Dest)%>%
  summarise(sum(Cancelled),sum(!Cancelled))

flights%>%
  group_by(Dest)%>%
  select(Cancelled)%>%
  table()%>%
  head()
  
  

head(aggregate(cbind(Cancelled, NotCancelled =!Cancelled)~Dest, data = flights, sum))


#Window Functions
#Aggregation function (like mean) takes n inputs and returns 1 value
#Window function takes n inputs and returns n values
#Includes ranking and ordering functions (like min_rank), 
#offset functions (lead and lag), and cumulative aggregates (like cummean).


# for each carrier, calculate which two days of the year they had their longest 
  #departure delays
# note: smallest (not largest) value is ranked as 1, so you have to use `desc` to 
  #rank by largest value

flights%>%
  group_by(UniqueCarrier)%>%
  select(Month, DayofMonth, DepDelay)%>%
  filter(min_rank(desc(DepDelay))<=2)%>%
  arrange(UniqueCarrier, desc(DepDelay))

# rewrite more simply with the `top_n` function
flights%>%
  group_by(UniqueCarrier)%>%
  select(Month, DayofMonth, DepDelay)%>%
  top_n(2)%>%
  arrange(UniqueCarrier, desc(DepDelay))
  

# for each month, calculate the number of flights and the change from the previous month
flights%>%
  group_by(Month)%>%
  summarise(flightCount = n())%>%
  mutate(changeFlight = flightCount-lag(flightCount))
  
  
# rewrite more simply with the `tally` function
flights %>%
  group_by(Month)%>%
  tally()%>%
  mutate(change = n-lag(n))



#Other Useful Convenience Functions

# randomly sample a fixed number of rows, without replacement
flights %>% sample_n(5)

# randomly sample a fraction of rows, with replacement
flights %>% sample_frac(0.01, replace=TRUE)


# base R approach to view the structure of an object
str(flights)
# dplyr approach: better formatting, and adapts to your screen width
glimpse(flights)



#Connecting to Databases
#dplyr can connect to a database as if the data was loaded into a data frame
#Use the same syntax for local data frames and databases
#Only generates SELECT statements
#Currently supports SQLite, PostgreSQL/Redshift, MySQL/MariaDB, BigQuery, MonetDB
#Example below is based upon an SQLite database containing the hflights data
#Instructions for creating this database are in the databases vignette
  #https://cran.r-project.org/web/packages/dplyr/vignettes/databases.html


# connect to an SQLite database containing the hflights data
my_db <- src_sqlite("my_db.sqlite3")


# connect to the "hflights" table in that database
flights_tbl <- tbl(my_db, "hflights")

# example query with our data frame
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay))

# identical query using the database
flights_tbl %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay)) 


#You can write the SQL commands yourself
#dplyr can tell you the SQL it plans to run and the query execution plan
# send SQL commands to the database
tbl(my_db, sql("SELECT * FROM hflights LIMIT 100"))


# ask dplyr for the SQL commands
flights_tbl %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay)) %>%
  explain()


