# Heatmap and graphs for SFO crime classification.
# Author - Anupama Rajaram
# Best score = 2.60


# -------------------- Section - 1 ------------------- #
# ---------------- Prep work and load data ----------- #
# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

# load standard libraries (not all are used, though)
library(data.table)
library(bit64)
library(dplyr)
library(readr)
library(ggplot2)
library(clusterSim)  # for normalization function.
library(nnet)
library(sqldf)

options(digits=7) # numeric values will have 7 decimal digits.
options(scipen=999) # probabilities will be in numeric notation, NOT scientific!
# e.g: p = 0.000043 NOT 4.3e-05


# load data
mytrain = fread("../input/train.csv", stringsAsFactors = TRUE)
# mytest = fread("../input/test.csv", stringsAsFactors = TRUE) 
# no need to load test datasets, since we are only performing visualization


# stripping Dates variable for date time manipulation
mytrain$dt = as.Date(mytrain$Dates)
mytrain$year = as.numeric(format(mytrain$dt, "%Y"))
mytrain$mth = as.numeric(format(mytrain$dt, "%m"))
mytrain$day = as.numeric(format(mytrain$dt, "%d"))


# --------------------- Section - 2 -------------------- #
# ------------ some descriptive statistics ------------- #
# this section is simply to explore the data, and can be completely ignored.

with(mytrain, table(DayOfWeek, Category))
# note both categories are converted to factors


# check how many unique values exist for each column.
sapply(mytrain, function(x) length(unique(x)))


# check for NAs - both datasets have 0 NA values, so we do not need
# to apply any corrections.
sapply(mytrain, function(x) sum(is.na(x)))
# sapply(mytest, function(x) sum(is.na(x)))

summary(mytrain)
head(mytrain) 

# ---------------- end of descriptive statistics ----------------- #






# --------------------- Section - 3 -------------------- #
# ------------------ some pretty graphs ---------------- #

# chart 1 -- crime category by day of week 
# image stored as CrimeCategory_by_DayOfWeek.jpg
ggplot(data=mytrain, aes(x=DayOfWeek)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~Category, scales='free')




# chart -- crime category by year 
# image file = Crimecount_byCategory&Yr.jpg
# note this image has a fixed scale, unlike the previous plot.
# hence we can clearly see the top 5 crime categories.
ggplot(data=mytrain, aes(x=year)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') +
  facet_wrap(~Category)


# chart -- simple chart showing criminal incidents per year
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

# chart showing crime rates by category and month.
# May, Oct, April are clearly months with highest incidents.
ggplot(data=mytrain, aes(x=mth)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') + 
  facet_wrap(~PdDistrict)


# considering only PdDistrict = Southern.
scrime = subset(mytrain, PdDistrict == "SOUTHERN")
ggplot(data=scrime, aes(x=year)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') + 
  facet_wrap(~Category)

ggplot(data=scrime, aes(x=mth)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') + 
  facet_wrap(~Category  )

ggplot(data=scrime, aes(x=DayOfWeek)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') + 
  facet_wrap(~Category)


# ---------------- end of graphs ----------------- #





# --------------------- Section - 3 -------------------- #
# ------------- generating a crime heatmap ------------- #

crimeSFO = sqldf("select PdDistrict as 'Area', 
                 Category as 'CrimeCategory', 
                 count(*) as 'count' 
                 from mytrain
                 group by PdDistrict, Category")


csfo = crimeSFO

# We do some subsetting to get counts for crime Category for each PdDistrict
# then we will merge it together as a matrix for the heatmap.
csfo1 = subset(csfo, Area == "BAYVIEW")
csfo1$Area = NULL
rownames(csfo1) <- csfo1[,1]

csfo2 = subset(csfo, Area == "CENTRAL")
csfo2$Area = NULL
rownames(csfo2) <- csfo2[,1]

csfo3 = subset(csfo, Area == "INGLESIDE")
csfo3$Area = NULL
rownames(csfo3) <- csfo3[,1]

csfo4 = subset(csfo, Area == "MISSION")
csfo4$Area = NULL
rownames(csfo4) <- csfo4[,1]

csfo5 = subset(csfo, Area == "NORTHERN")
csfo5$Area = NULL
rownames(csfo5) <- csfo5[,1]

csfo6 = subset(csfo, Area == "PARK")
csfo6$Area = NULL
rownames(csfo6) <- csfo6[,1]

csfo7 = subset(csfo, Area == "RICHMOND")
csfo7$Area = NULL
rownames(csfo7) <- csfo7[,1]

csfo8 = subset(csfo, Area == "SOUTHERN")
csfo8$Area = NULL
rownames(csfo8) <- csfo8[,1]

csfo9 = subset(csfo, Area == "TARAVAL")
csfo9$Area = NULL
rownames(csfo9) <- csfo9[,1]


csfo10 = subset(csfo, Area == "TENDERLOIN")
csfo10$Area = NULL
rownames(csfo10) <- csfo10[,1]


ctt1 = cbind(csfo1, csfo2, csfo4, csfo5)
ctt1 = ctt1[, c(2,4,6,8)]

ctt2 = cbind(csfo7, csfo8, csfo9, csfo10)
ctt2 = ctt2[, c(2,4,6,8)]

ctt3 = cbind(csfo3, csfo6)
ctt3 = ctt3[, c(2,4)]

mcol = merge(ctt1, ctt2, by="row.names", all.x = TRUE)
rownames(mcol) <- mcol[,1]
mcol$Row.names = NULL

mcol = merge(mcol, ctt3, by="row.names", all.x=TRUE)
rownames(mcol) <- mcol[,1]
mcol$Row.names = NULL
mcol[is.na(mcol)] = 0

colnames(mcol) = c('Bayview', 'Central', 'Mission', 'Northern',
                   'Richmond', 'Southern', 'Taraval', 'Tenderloin',
                   'Ingleside', 'Park')


m1 = data.matrix(mcol)
# creating a matrix to use with heatmaps.

# basic heatmap - orange and white show higher crime incidents
sfo_hmap1 <- heatmap(m1,Rowv=NA, Colv=NA, 
                     col = heat.colors(256), 
                     margins=c(5,10) , 
                     main = 'Heatmap of crime category versus Area')

# blue heatmap - pink and white indicate more crime.
sfo_hmap <- heatmap(m1,Rowv=NA, Colv=NA, 
                    col = cm.colors(256), scale="row", 
                    margins=c(5,10) ,
                    main = "Blue heatmap for SFO crime classification")

