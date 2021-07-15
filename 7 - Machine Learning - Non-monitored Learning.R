library(ggplot2)
library(cowplot)
library(viridis)
library(scales)
library(readxl)
library(data.table)
library(ape)
library(MASS)
library(matrixStats)
setwd("F:/R directories")
options(width = 70, digits = 2)
# This part concerns the unsupervised machine learning. The target of this type
# of learning is the untagged data, with the purpose of classifying the data
# according to certain characteristics so that every class shares identical 
# features. 
# This nature makes it a good way of data dimensionality reduction: if a data 
# consists of hundreds or thousands of parameters but only a few thousands of 
# observations, we might hope to shrink this numerous parameters to a smaller 
# number (i.e. dimensionality reduction). The reduced data still involves most
# of the information from the original data. Hence, unsupervised learning becomes
# the final phase of explorational data analysis.
# Unsupervised learning also helps determining the number of unique groups or
# dimensions within the data, locating various similarities among different
# observations.

# Tagged or untagged, data usually comes in a chaotic status, which requires
# great effort in pre-processing. It's more than usual that we encounter the
# data with the format incomparable to machine learning. Therefore, this part 
# also covers the ways of transforming data into proper formats.

# This part also introduces principal component analysis (PCA). Although R has
# implemented the PCA function, the pcaMethods package provides more options.
# We use the following code to load this package.
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("pcaMethods")
library(pcaMethods)

# To begin with, we load the data into the environment. Here, the data is the 
# gender statistics published by the World Bank, with omissions.
dRaw <- read_excel("Gender_StatsData_worldbank.org_ccby40.xlsx")
dRaw <- as.data.table(dRaw)
# The very first step of any data analysis is to know about the data structure.
# For example, we should at least know the types and arrangements of the data.
# By using the str() on a dataset, we can acquire the structure of target data.
# In this case, most entries are in the numeric form.
str(dRaw)
# Meanwhile, the summary() function generates rudimentary summaries on columns
# of the dataset. Here we realize the data is not in the proper form for machine
# learning: a column consists of different classes of measurements. In addition,
# there is no means to acquire information regarding the years from a variable.
summary(dRaw)
# To better understand the label of the data (hereby the names of countries),
# the unique() function can come in handy.
unique(dRaw$CountryName)
unique(dRaw$IndicatorCode)
# Before we apply machine learning to the data, we need to rearrange them so 
# every column only consists of the value of one indicator.