setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
rm(list = ls())
library(dplyr)

# familyPreCYFData <- read.csv("Data/Data/FamilyPreCYFData.csv")
# 
# source("Functions/CalTypeCounts.R")
# typeCountsData <- calTypeCounts(familyPreCYFData)

typeCountsData <- read.csv("Data/TypeCountsData.csv")

familyPlacePostCYFData <- read.csv("Data/FamilyPlacePostCYFData.csv")
durationAndCloseTimes <- read.csv("Data/DurationAndCloseTimes.csv")

source("Functions/MergeXYVars.R")
typeCountsFinalData <- mergeXYVars(typeCountsData, familyPlacePostCYFData, durationAndCloseTimes)

# typeCountsFinalData <- read.csv("Data/TypeCountsFinalData.csv")
