# SESSION 1 ----------------------------------------------------------------------------

# BASICS --------------------------------------------------------------

# R as a calculator
3+5
3*4
(8/2)-(3*4)
pi
log(27)
4%%2

# Variable assignment and vectors
x <- c(1,2,7.4,-2,0) # A numeric vector
y <- c("one","two","three") # A character vector
z <- c(TRUE, FALSE, TRUE) # A logical vector

x

?str
str(x)
str(y)

# Vectorized operations
x <- c(1,4,7)
y <- seq(from=2,to=6,by=2) #Note that this is the same as seq(2,6,2)

x
x/2
x^2
sqrt(x)

y
x-y
x*y

# Combine vectors
w <- c(x, y)
w

# Working with data frames
z <- matrix(seq(1,12),3,4)
z[2:3, 3:4]
z[,2:3]
z[,1]
z[,1,drop=FALSE]

# Reading in data & libraries/packages
?read.csv
salaries_data <- read.csv("Salaries.csv")

# head(salaries_data)
# head(salaries_data, 15)

# install.packages("haven")
library(haven)

wage_data <- data.frame(read_dta("MROZ.dta"))

# Inspecting data
head(wage_data)
tail(wage_data)
summary(wage_data)

nrow(wage_data)
ncol(wage_data)

names(wage_data)

str(wage_data)
dim(wage_data)

wage_data[2,]
wage_data[,3]
wage_data[2,1]

wage_data[c(1:3),]
wage_data[,c(1:2,length(wage_data)-1)]

names(wage_data)

wage_data[,c("age", "educ", "exper")]

# Lists
my_list <- list()

my_list$a_vector <- c(1,2,3)
my_list$a_matrix <- matrix(seq(1,9),3,3)
my_list$a_dataframe <- wage_data

# Base R functions and more operations
wage_data$wage
wage_data$lwage

summary(wage_data$hours)
mean(wage_data$wage)
sd(wage_data$wage)
hist(wage_data$wage)
hist(wage_data$wage, breaks=10)

#Some basic plotting
?plot
plot(wage_data$age, wage_data$wage)

# Simple regression
lm(wage~educ, wage_data)
fit <- lm(wage~educ, wage_data)
fit
summary(fit)

plot(wage~educ, wage_data)
abline(fit, col="firebrick")

fit2 <- lm(wage~educ + exper, wage_data)
names(fit2)

confint(fit2)

# Also have things like cor, cov, var, etc...

# Conditionals
1 < 2
x < y
x == y
1 != 1

x %in% z

# Conditionals and subsetting
salaries_data[salaries_data$rank == "Prof",]
salaries_data[salaries_data$sex == "Female",]
salaries_data[salaries_data$salary > 100000,]

# BASICS - PRACTICE ---------------------------------------------------

# Using subsetting, drop the `X` index column
# Create both a histogram and a boxplot of the `salary` variable
# What proportion of the professors in the dataset are Female?
# Conduct a simple linear regression of  `yrs.service` on `salary`
# Report the coefficients, standard errors, and confidence interval for the regression specified above
# Create a simple plot of `yrs.service` and `salary`
# Using `?plot`, create properly formatted labels and titles for the plot above
# What is the average salary of the `AssocProf` rank?
# Compute the standard deviations in salary for male and female professors, separately
# Which `discipline` has the higher median salary?
# How many years of service does the 200th individual in this dataset have?

salaries_data <- salaries_data[2:7]


hist(salaries_data$salary)


boxplot(salaries_data$salary)


nrow(salaries_data[salaries_data$sex=="Male",])/nrow(salaries_data)


fit <- lm(salary ~ yrs.service, salaries_data)


fit$coefficients


summary(fit)


confint(fit)


plot(x = salaries_data$yrs.service, y = salaries_data$salary)


plot(x = salaries_data$yrs.service, y = salaries_data$salary, xlab = "Years of service", ylab = "Salary", main = "Salary by years of service")


mean(salaries_data[salaries_data$rank=="AssocProf","salary"])


sd(salaries_data[salaries_data$sex=="Female","salary"])


sd(salaries_data[salaries_data$sex=="Male","salary"])


salaries_data[200, "yrs.service"]

# SESSION 2 ----------------------------------------------------------------------------

rm(list=ls())

# INSTALL AND/OR ATTACH THESE LIBRARIES
library(dplyr)
library(haven)

# WARM-UP -------------------------------------------------------------

# Read the 'state_unemp_clean.csv' data into memory and assign it to a variable of your choosing
# Convert the 'date' column to the date type (Hint: Use 'as.Date' and reassign it to the 'date' variable)
# What is the highest unemployment rate in the sample?
# Which state has the highest unemployment rate in the sample? In what year was that rate reached?
# What is the average household income across all the states in the sample?
# What is Ohio's average unemployment rate in this time horizon?
# Create a time-series plot of the unemployment rate in the state with the lowest unemployment rate in 2016
# Change the x- and y-labels and plot title to descriptive names
# Using '?plot' for help, change the type of the plot to a line graph

# ---------------------------------------------------------------------

state_unemp <- read.csv("state_unemp_clean.csv", stringsAsFactors=FALSE)


state_unemp$date <- as.Date(state_unemp$date)


max(state_unemp$Unemployment_rate)


state_unemp$State[state_unemp$Unemployment_rate == max(state_unemp$Unemployment_rate)]


state_unemp$date[state_unemp$Unemployment_rate == max(state_unemp$Unemployment_rate)]


mean(state_unemp$Median_Household_Income_2015)


mean(state_unemp$Unemployment_rate[state_unemp$State == "OH"])


unemp_2016 <- state_unemp[state_unemp$date == "2016-01-01",]
unemp_2016$State[unemp_2016$Unemployment_rate == min(unemp_2016$Unemployment_rate) & 
                   unemp_2016$date == "2016-01-01"]


plot(state_unemp$date[state_unemp$State == "NH"], state_unemp$Unemployment_rate[state_unemp$State == "NH"])


plot(state_unemp$date[state_unemp$State == "NH"], 
     state_unemp$Unemployment_rate[state_unemp$State == "NH"],
     xlab = "Year", ylab = "Unemployment rate", main = "New Hampshire Unemployment Rate",
     type = "l")


# ---------------------------------------------------------------------

# Load in data
wage_data <- data.frame(read_dta("MROZ.dta"))
salaries_data <- read.csv("Salaries.csv", stringsAsFactors=FALSE)
county_unemp <- read.csv("county_unemp_clean.csv", stringsAsFactors=FALSE)

# PROGRAMMING IN R ----------------------------------------------------

a <- 7

if (a %% 2 == 0) {
  print("even")
} else {
  print("odd")
}

# Control flow
a <- 7

if(a%%2 == 0) {
  print("even")
} else {
  print("odd")
}

ifelse(a%%2 == 0, "even", "odd")

# Functions
square <- function(x) {
  return(x**2)
} 

square(3)
square(x)

# Loops - extending our mix of control flow and conditionals above
a_vector <- c(1,6,7,8,8293,21,888,3,-2)

for(i in 1:length(a_vector)){
  if(a_vector[i]%%2 == 0) {
    print("even")
  } else {
    print("odd")
  }
}

# Specify a random vector using the following syntax
rand_vect <- round(100*runif(100),0)

# PROGRAMMING IN R - PRACTICE -----------------------------------------

# Write a function `cube`, which takes a value and returns that value cubed; write a loop to apply this function to all the elements of the vector; print the cubed values
# Using a loop and control flow, check if each element of the vector is a perfect square, if it is return the index, i, and print "Perfect square!"
# Hint: Use x%%1 to check if your number is a whole number

# Load the 'salaries_list.Rds' object into memory (`readRDS()`)
# Inspect the list; what does each element contain? What is distinct about them?
# Loop through the list, and return the average of the `yrs.since.phd` variable for each element

cube <- function(x) {
  return(x**3)
}

for(i in 1:length(rand_vect)) {
  print(cube(rand_vect[i]))
}

for(i in rand_vect) {
  print(cube(i))
}

for(i in 1:length(rand_vect)) {
  if(sqrt(rand_vect[i])%%1 == 0) {
    print(i)
    print("Perfect square!")
  }
}

# DATA ANALYSIS -------------------------------------------------------

# Dealing with missing values
a_dataframe <- data.frame(x = c("x", "y", "z"), 
                          y2015 = c(1,2,3), 
                          y2016 = c(9,10,3), 
                          y2017=c(10,NA,3), 
                          stringsAsFactors = FALSE)

summary(a_dataframe)
sum(is.na(a_dataframe))
a_dataframe[complete.cases(a_dataframe),]

library(readxl)
hpi <- data.frame(read_excel("HPI_AT_BDL_county.xlsx", na=c("", "."), skip=6))

colnames(hpi) <- c("state", "county", "FIPStxt", "year", "annual_change", "hpi", "hpi_1990", "hpi_2000")
hpi[,c(5:8)] <- (sapply(hpi[c(5:8)], as.numeric))

summary(hpi)
sum(is.na(hpi))
hpi[complete.cases(hpi),]

# dplyr ---------------------------------------------------------------

# dplyr basics
head(salaries_data)
summary(salaries_data)

select(salaries_data, rank, discipline, yrs.since.phd, yrs.service, sex, salary)
select(salaries_data, c(1,2))
select(salaries_data, starts_with("yrs"))

salaries_data2 <- select(salaries_data, -X)

filter(salaries_data2, rank=="Prof")
filter(salaries_data2, salary>=100000)
filter(salaries_data2, salary>=100000 & sex=="Female")
filter(hpi, !is.na(hpi))

arrange(salaries_data2, salary)
arrange(salaries_data2, desc(salary))
arrange(salaries_data2, sex, rank)

mutate(salaries_data2, bonus="yes")
salaries_data2 <- mutate(salaries_data2, bonus=ifelse(yrs.service>=10, 10000, 2000))

salaries_data2
salaries_data2 <- mutate(salaries_data2, salary=salary + bonus)

salaries_data %>%
  select(-X) %>%
  mutate(bonus=ifelse(yrs.service>=10, 10000, 2000)) %>%
  mutate(salary = salary + bonus) %>%
  select(-bonus) %>%
  arrange(salary) %>%
  filter(sex=="Female")

# dplyr merges---------------------------------------------------------

# Merging, pipes, and group_by
data(state)
str(state.x77)
str(state.region)
str(state.name)

# cbind and rbind - not merges
x <- seq(1:10)
y <- letters[seq(1:10)]
z <- rep(c(TRUE,FALSE), 5)

rbind(x,y,z)
cbind(x,y,z)
data.frame(x,y,z)

cbind(state.name, state.region)
cbind(state.name, as.character(state.region))
cbind(sort(state.name,TRUE), as.character(state.region))

name_region_map <- data.frame(state.name, as.character(state.region))
names(name_region_map) <- c("state", "region")

state.x77
state_info <- cbind(rownames(state.x77), data.frame(state.x77, row.names=NULL))
colnames(state_info)[1] <- "state"

state_info
merge(state_info, name_region_map, by="state")
left_join(state_info, name_region_map, by="state")

left_data <- data.frame(country = c("A", "B", "B", "C", "D", "E", "F"), 
                        left_value = c(1380, 8023, 12301, 682, 542, 1042, 972),
                        stringsAsFactors = FALSE)

right_data <- data.frame(country = c("B", "D", "E", "F", "G", "H", "I"),
                         right_value = c(2.2, 7.8, 12.4, 9.2, 18.3, 1.7, 10.1),
                         stringsAsFactors = FALSE)

#left_join - mutating - match, by key, right-hand data to the left-hand data
left_join(left_data, right_data, by="country")

#right_join - mutating - match, by key, left-hand data to the right-hand data
right_join(left_data, right_data, by="country")
left_join(right_data, left_data, by="country")

#inner_join - mutating - match, by key, and keep only those observations found in both datasets
inner_join(left_data, right_data, by="country")

#full_join - mutating - match, by key, and keep all observations, regardless of which dataset they're found in
full_join(left_data, right_data, by="country")

#semi_join - filtering - match, by key, keeping only the left-hand data, and filter out those which don't have a match in right-hand data
semi_join(left_data, right_data, by="country")

#anti_join - filtering - match, by key, keeping only the left-hand data, and filter out those which do have a match in right-hand data
anti_join(left_data, right_data, by="country")

head(state_unemp)
state_unemp %>%
  select(Area_name, Unemployment_rate) %>%
  group_by(Area_name) %>%
  summarise(Unemployment_rate = mean(Unemployment_rate, na.rm=TRUE))

# DATA ANALYSIS - PRACTICE --------------------------------------------

weo_report <- read.csv("weo_clean.csv", stringsAsFactors = FALSE)
wdi_indicators <- read.csv("wdi_indicators.csv", stringsAsFactors = FALSE)

# Merge in the World Development Indicators indicators data with the WEO data
# Report countries that did not receive valid region or income_group identifers

# Using dplyr and pipes, in one chained command, create a subset of the data that:
# Has only those observations from the "Europe & Central Asia" region from 2016
# Has only the country, gdppc, unemployment_rate, and curr_acc_bal values

# Change the units of unemployment_rate to reflect a percent with a mutate command (divide by 100)
# Find the average unemployment rate for this group
# Create a time-series plot of Danish unemployment over the sample period
# Find the average level of GDP for each income group
# Which region has the largest within-region disparity in GDP per capita, as measured by standard deviation?
# Create histograms of the GDP per capita variable within each region, setting the title of each plot to the name of the region

weo_full <- left_join(weo_report, wdi_indicators, by="country")


unmerged_countries <- filter(weo_full, is.na(region))
unique(unmerged_countries$country)


weo_full <- filter(weo_full, !is.na(region))


eca_weo <-
  weo_full %>%
  filter(region == "Europe & Central Asia" & year == 2016) %>%
  select(country, gdppc, unemployment_rate, curr_acc_bal) %>%
  mutate(unemployment_rate = unemployment_rate/100) %>%
  summarise(mean_unemployment_rate = mean(unemployment_rate, na.rm=TRUE))

eca_weo


dmk_ur <- 
  weo_full  %>%
  filter(country=="Denmark") %>%
  select(country, year, unemployment_rate)

plot(dmk_ur$year, dmk_ur$unemployment_rate, "l")


weo_full %>%
  group_by(income_group) %>%
  summarise(deviation = sd(gdp_cp, na.rm=T)) %>%
  ungroup() %>%
  filter(deviation == max(deviation))

for (region in unique(weo_full$region)) {
  hist(weo_full$gdp_cp[weo_full$region == region], main = region, xlab = "GDP per capita")
}

# SESSION 3 ----------------------------------------------------------------------------

# Set working directory and libraries
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)
library(ggplot2)

# EXTRAS --------------------------------------------------------------

# Dates

Sys.Date()

as.Date("2018-01-01")
class(as.Date("2018-01-01"))
class("2018-01-01")

date1 <- "Jan 01 2018"
date2 <- "1/12/2018"
date3 <- "Monday, January 15, '18"
class(as.Date(date1, format="%b %d %Y"))
class(as.Date(date2, format="%m/%d/%Y"))

mdy(date1)
mdy(date2)
dmy(date3) # Not a solution for everything!

# Strings

state_unemp <- read.csv("state_unemp_clean.csv", stringsAsFactors=FALSE)
county_unemp <- read.csv("county_unemp_clean.csv", stringsAsFactors=FALSE)

glimpse(state_unemp)
glimpse(county_unemp)

names_vector <- colnames(state_unemp)

grep("Unem", names_vector)
grepl("Unem", names_vector)
str_detect(names_vector, "Unem")

names_vector[str_detect(names_vector, "Unem")]

gsub("_2013", "", names_vector)
str_replace(names_vector, "_2013", "")

paste("Hi", "there!")
paste0("Hi", "there!")

# The 'apply' family

char_vector <- as.character(c(seq(1:12)))
char_df <- data.frame(x=char_vector[1:4], y=char_vector[5:8], z=char_vector[9:12], stringsAsFactors = FALSE)
char_df
str(char_df)

for(i in 1:length(char_df)) {
  char_df[,i] <- as.numeric(char_df[,i])
}

str(char_df)

# Simplifies the underlying data structure
sapply(char_df, as.numeric)

# Always returns a list
lapply(char_df, as.numeric)
lapply(wage_data2[,c(2:length(wage_data2))], regplot, wage_data2$wage)

sapply(a_vector, function(x) ifelse(x%%2 == 0, "even", "odd"))

# tidyr functions

# The "gather" function
a_dataframe <- data.frame(ids = c("x", "y", "z"), y2015 = c(1,2,3), y2016 = c(9,10,3), y2017=c(10,NA,3), stringsAsFactors = FALSE)
str(a_dataframe)
a_dataframe

year_variables <- colnames(a_dataframe)[2:4]
year_data_all <- list()
for(i in 1:length(year_variables)) {
  year_data <- select(a_dataframe, 1, year_variables[i])
  year_data$year <- year_variables[i]
  names(year_data) <- c("ids", "value", "year")
  year_data_all[[i]] <- year_data
}

data.frame(Reduce("rbind", year_data_all))

data_long <- gather(a_dataframe, key=year, value=value, y2015:y2017)
data_long

# Back to wide
spread(data_long, key = year, value=value)

# Using 'union' and 'separate'
bigger_dataframe <- data.frame(ids = c("x", "x", "x", "y", "y", "y", "z", "z", "z"), 
                               vars = c("a","b","c","a","b","c","a","b","c"),
                               y2015 = c(1,2,3,4,5,6,7,8,9), 
                               y2016 = c(9,10,3,90,7,2,NA,8,1), 
                               y2017=c(10,NA,3,10,NA,3,10,NA,3), 
                               stringsAsFactors = FALSE)

data_long <- gather(bigger_dataframe, key=year, value=value, y2015:y2017)
spread(data_long, key=vars, value=value)

# Using real data
weo_raw <- read.csv("weo_report.csv", na.strings=c(""), stringsAsFactors=FALSE)
weo_raw$X2000 <- as.numeric(weo_raw$X2000)
weo_raw$X2002 <- as.numeric(weo_raw$X2002)
weo_raw$X2012 <- as.numeric(weo_raw$X2012)

weo_long <- gather(weo_raw, key=year, value=value, X2000:X2016)
weo_clean <- spread(weo_long, key=Subject.Descriptor, value=value)
weo_clean$year <- as.numeric(gsub("X", "", weo_clean$year))

write.csv(weo_clean, "weo_clean.csv", row.names=FALSE)

# ggplot2 -------------------------------------------------------------

# Grab one year/one country of data
weo_2016 <- weo_full[weo_full$year == 2016,]
weo_country <- weo_full[weo_full$country == "Argentina",]

ggplot(weo_2016, aes(x=unemployment_rate)) +
  geom_histogram(bins=30) +
  labs(x = "Unemployment rate (%)", y = "Frequency", title = "Distribution of unemployment rates, 2016")

ggplot(weo_2016, aes(x=unemployment_rate, y=gdp_cp)) +
  geom_point() +
  labs(x = "Unemployment rate (%)", y = "GDP per capita", title = "Unemployment vs. GDP per capita, 2016")

ggplot(weo_country, aes(x=year, y=unemployment_rate)) +
  geom_point() +
  labs(x = "Year", y = "Unemployment rate (%)", title = "Argentinian unemployment over time")

ggplot(weo_country, aes(x=year, y=unemployment_rate)) +
  geom_point() +
  geom_line() + 
  labs(x = "Year", y = "Unemployment rate (%)", title = "Argentinian unemployment over time")

# Grab a few countries
weo_countries <- weo_full[weo_full$country %in% c("Argentina", "Brazil", "Chile", "Uruguay"),]

ggplot(weo_countries, aes(x=year, y=unemployment_rate, col=country)) +
  geom_line(size=1) +
  labs(x = "Year", y = "Unemployment rate (%)", title = "Unemployment rate in Latin American countries")

# Grab a couple of years
weo_years <- weo_full[weo_full$year %in% c(2014,2015),]

ggplot(weo_years, aes(x=unemployment_rate, fill=factor(year))) +
  geom_histogram(bins=50) +
  labs(x = "Unemployment rate (%)", y = "Frequency", title = "Distribution of unemployment, by year") +
  scale_fill_discrete(name = "Year")

# Side-by-side
ggplot(weo_years, aes(x=unemployment_rate, fill=factor(year))) +
  geom_histogram(bins=50) +
  facet_wrap(~factor(year)) +
  labs(x = "Unemployment rate (%)", y = "Frequency", title = "Distribution of unemployment, by year") +
  scale_fill_discrete(name = "Year")

# ggplot2 - PRACTICE --------------------------------------------------

# Plot the GDP per capita values for the `Europe & Central Asia` region over time, with each country as a separate color; label accordingly
# Using `dplyr` commands (and pipes, if possible), plot the average `unemployment_rate` over time, with each region as its own color; label accordingly