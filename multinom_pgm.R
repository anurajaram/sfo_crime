# Author - Anupama Rajaram
# score = 2.65783 (without dates manipulation), output file = multinom.csv
# score = 2.60294, with complex multinomial regression . file = multinom_dates.csv
# Only an improvement of 0.05 but pushed me up by 218 spots! Woohoo! 



# -------------------- Section - 1 ------------------- #
# ---------------- Prep work and load data ----------- #
# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

# load standard libraries (not all are used, though)
library(data.table)
library(bit64)
library(e1071)
library(nnet)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

options(digits=7) # numeric values will have 7 decimal digits.
options(scipen=999) # probabilities will be in numeric notation, NOT scientific!
# e.g: p = 0.000043 NOT 4.3e-05


# load data - I prefer fread() since it is much faster than read.csv()
mytrain = fread("train.csv", stringsAsFactors = TRUE)
mytest = fread("test.csv", stringsAsFactors = TRUE)


# stripping Dates variable for date time manipulation
mytrain$dt = as.Date(mytrain$Dates)
mytrain$year = as.numeric(format(mytrain$dt, "%Y"))
mytrain$mth = as.numeric(format(mytrain$dt, "%m"))
mytrain$day = as.numeric(format(mytrain$dt, "%d"))

# similar process for mytest dataset
mytest$dt = as.Date(mytest$Dates)
mytest$year = as.numeric(format(mytest$dt, "%Y"))
mytest$mth = as.numeric(format(mytest$dt, "%m"))
mytest$day = as.numeric(format(mytest$dt, "%d"))







# --------------------- Section - 2 -------------------- #
# ------------ some descriptive statistics ------------- #
# this section is simply to explore the data, and can be completely ignored.

with(mytrain, table(DayOfWeek, Category))
# note both categories are converted to factors


# check how many unique values exist for each column.
sapply(mytrain, function(x) length(unique(x)))

sapply(mytest, function(x) length(unique(x)))
# output 
# Id          Dates       DayOfWeek PdDistrict    Address     X 
# 884262     392173          7         10         23184      34346 
# Y             dt       year        mth        day 
# 34346       2266         13         12         31

# check for NAs - both datasets have 0 NA values, so we do not need
# to apply any corrections.
sapply(mytrain, function(x) sum(is.na(x)))
sapply(mytest, function(x) sum(is.na(x)))

summary(mytrain)
head(mytest) 

# ---------------- end of descriptive statistics ----------------- #






# --------------------- Section - 3 -------------------- #
# ------------------ some pretty graphs ---------------- #
# -------- this section can also be ignored ------------ #

# chart 1 -- crime category by day of week 
# image stored as CrimeCategory_by_DayOfWeek.jpg
ggplot(data=mytrain, aes(x=DayOfWeek)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~Category, scales='free')

# chart -- crime category by day of year 
# image file = Crimecount_byCategory&Yr.jpg
# note this image has a fixed scale, unlike the previous plot.
ggplot(data=mytrain, aes(x=year)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') +
  facet_wrap(~Category)


# chart -- criminal incidents per year
ggplot(data=mytrain, aes(x=year)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') 


# chart -- crime category by month  
ggplot(data=mytrain, aes(x=mth)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') +
  facet_wrap(~Category)


# chart -- districts with highest crimerates = Southern, Mission and Northern,  
# in descending order respectively.
ggplot(data=mytrain, aes(x=year)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') + 
  facet_wrap(~PdDistrict)


ggplot(data=mytrain, aes(x=mth)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') + 
  facet_wrap(~PdDistrict)
# May, Oct, April are months with highest incidents.

# ---------------- end of graphs ----------------- #





# --------------------- Section - 4 -------------------- #
# ------- modelling using multinomial regression ------- #

# multinomial regression as predictive model
mod <- multinom(Category ~ DayOfWeek + year + mth + PdDistrict, 
                data = mytrain, maxit = 500)
# this takes almost ~2 hours to complete calculation on a Windows 10 laptop 
# with 4GB accessible to RStudio application.
# score = 2.60

# simple model and loads faster - commented out for now.
# this formula gives score of 2.65, and takes ~30 minutes to finish computation
# for 100 iterations.
# mod <- multinom(Category ~ DayOfWeek PdDistrict, data = mytrain)


# predict the probabilities "probs" associated with each option 
# and then draw a random number to make our actual selection
opfinal = predict(mod,mytest,"probs")
crimeopfinal = data.frame(opfinal)
submitdf = cbind(mytest$Id, crimeopfinal)

# format column headers to match submission requirements
colnames(submitdf) = c('Id', 'ARSON','ASSAULT','BAD CHECKS','BRIBERY',
                   'BURGLARY','DISORDERLY CONDUCT','DRIVING UNDER THE INFLUENCE',
                   'DRUG/NARCOTIC','DRUNKENNESS','EMBEZZLEMENT','EXTORTION',
                   'FAMILY OFFENSES', 'FORGERY/COUNTERFEITING','FRAUD',
                   'GAMBLING','KIDNAPPING','LARCENY/THEFT',
                   'LIQUOR LAWS','LOITERING','MISSING PERSON','NON-CRIMINAL',
                   'OTHER OFFENSES', 'PORNOGRAPHY/OBSCENE MAT','PROSTITUTION',
                   'RECOVERED VEHICLE','ROBBERY',
                   'RUNAWAY','SECONDARY CODES','SEX OFFENSES FORCIBLE',
                   'SEX OFFENSES NON FORCIBLE',
                   'STOLEN PROPERTY','SUICIDE','SUSPICIOUS OCC','TREA',
                   'TRESPASS','VANDALISM',
                   'VEHICLE THEFT','WARRANTS','WEAPON LAWS')

# summary(submitdf[1])  # simply for checking

# create submission file.
write.csv(submitdf, file = "multinom.csv", row.names = FALSE)

