# Author - Anupama Rajaram
#
# Program description - Exploring if relationships exist between factors affecting 
#                       SFO crime categories.
#                       Hypothesis check using chisquare tests, correlograms and covariance.
#
# Dataset - (Kaggle) SFO crime classification
# Best score = 2.60

# Heatmap and graphs for SFO crime classification.
# Author - Anupama Rajaram
# Best score = 2.60


# -------------------- Section - 1 ------------------- #
# ---------------- Prep work and load data ----------- #
# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

# load standard libraries (not all are used, though)
library(data.table)
library(readr)
library(ggplot2)
library(clusterSim)  # for normalization function.
library(sqldf)
library("vcd") # for chi-sq tests
library(corrgram) # view correlogram
library(PerformanceAnalytics)  # for correlation chart function

options(digits=7) # numeric values will have 7 decimal digits.
options(scipen=999) # probabilities will be in numeric notation, NOT scientific!
# e.g: p = 0.000043 NOT 4.3e-05


# load data
mytrain = fread("train.csv", stringsAsFactors = TRUE)
# mytest = fread("../input/test.csv", stringsAsFactors = TRUE) 
# no need to load test datasets, since we are only performing visualization


# stripping Dates variable for date time manipulation
mytrain$dt = as.Date(mytrain$Dates)
mytrain$year = as.numeric(format(mytrain$dt, "%Y"))
mytrain$mth = as.numeric(format(mytrain$dt, "%m"))
mytrain$day = as.numeric(format(mytrain$dt, "%d"))




# --------------------- Section - 3 -------------------- #
# ----------------- chisquare tests -------------------- #

# chisq relations to check relationship between crime-category & other variables

# Null hypothesis 1 (Ho1) - no relation between crime-category and weekday (M/Tue/W/Th.. )
chisq1 <- xtabs(~Category + DayOfWeek, data = mytrain)
chisq.test(chisq1)
# relationship exists, Ho1 rejected.
# X-squared = 6057.7, df = 228, p-value < 2.2e-16

# Null hypothesis 2 (Ho2) - no relation between crime-category and SFo-district (Bayview, Southern, etc.)
chisq2 <- xtabs(~Category + PdDistrict, data = mytrain)
chisq.test(chisq2)
# relationship exists, Ho2 rejeced.
# X-squared = 125420, df = 342, p-value < 2.2e-16

# Null hypothesis 3 (Ho3) - no relation between crime-category and x-coordinate.
chisq.test( xtabs(~Category + X, data = mytrain))
# relationship exists, Ho3 rejected.
# X-squared = 12763300, df = 1301200, p-value < 2.2e-16

# Null hypothesis 4 (Ho4) - no relation between crime-category and year 
chisq.test( xtabs(~Category + year, data = mytrain))
# relationship exists.
# X-squared = 37265, df = 456, p-value < 2.2e-16

# Null hypothesis 5 (Ho5) - no relation between rrime-category and month.
chisq.test( xtabs(~Category + mth, data = mytrain))
# relationship exists.
# X-squared = 1845.3, df = 418, p-value < 2.2e-16

# Null hypothesis 6 (Ho6) - no relation between rrime-category and weekday (M/Tue/W/Th.. )
mytable <- xtabs(~Category+PdDistrict+year, data = mytrain)
mantelhaen.test(mytable)  
# realtion exists. Cochran-Mantel-Haenszel M^2 = 122710, 
# df = 342, p-value < 2.2e-16

# Null hypothesis 7 (Ho7) - no relation between rrime-category and weekday (M/Tue/W/Th.. )
mytable2 <- xtabs(~Category+PdDistrict + mth, data = mytrain)
mantelhaen.test(mytable2)  
# realtion exists. Cochran-Mantel-Haenszel M^2 = 122710, 
# df = 342, p-value < 2.2e-16

assocstats(mytable) 
# year 2010, 2012, 2013 have strong relationships as seen from 
# value of Contingency coefficients.
# other years have NaN values indicating there is not enough data
# or a weak relationship.

assocstats(chisq1)

# ----------------- end of section --------------------- #




# --------------------- Section - 4 -------------------- #
# -------------------- CORRELOGRAMS -------------------- #
# -------- CREATE CORRELATION MATRIX DIAGRAM ----------- #
corrgram(mytrain)
# image saved as corr_matrx_quant.jpg.
# note this function only takes quantitative variables, hence
# variables like crime category and DayOfWeek are missing.

# To get a better correlation matrix, we convert some of the variables
# from "factors/ strings" to "integers".
myt = mytrain
myt$DayOfWeek = as.integer(myt$DayOfWeek)
myt$PdDistrict = as.integer((myt$PdDistrict))
myt$Category = as.integer(myt$Category)

# delete unnecesary columns.
myt$Dates = NULL
myt$Resolution = NULL
myt$Address = NULL
myt$dt = NULL
myt$Descript = NULL

chart.Correlation(cor(myt), histogram=TRUE) 
# image saved as cov_variables.jpg
 
corrgram(myt)  #image saved as corr_categ_vars.jpg
 
 
 
