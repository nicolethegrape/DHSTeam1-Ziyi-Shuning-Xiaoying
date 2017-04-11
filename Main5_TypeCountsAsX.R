setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
rm(list = ls())
library(dplyr)

## familyPreCYFData <- read.csv("Data/Data/FamilyPreCYFData.csv")
## 
## source("Functions/CalTypeCounts.R")
## typeCountsData <- calTypeCounts(familyPreCYFData)

# typeCountsData <- read.csv("Data/TypeCountsData.csv")
# 
# familyPlacePostCYFData <- read.csv("Data/FamilyPlacePostCYFData.csv")
# durationAndCloseTimes <- read.csv("Data/DurationAndCloseTimes.csv")
# 
# source("Functions/MergeXYVars.R")
# typeCountsFinalData <- mergeXYVars(typeCountsData, familyPlacePostCYFData, durationAndCloseTimes)

typeCountsFinalData <- read.csv("Data/TypeCountsFinalData.csv")

## Temporarily put graph codes here

# bar plot about types of services and placement
library(ggplot2)
ggplot(typeCountsFinalData, aes(TypeCounts, fill = PlacementAsY, order = -as.numeric(PlacementAsY))) + 
  geom_bar(stat = "bin", position = "fill", alpha=0.6) + 
  xlab("Types of Services") +
  ylab("Place / Not Place Percentages") +
  ggtitle("Types of Services and Placement")


# bar plot about types of services and close times
# change CloseTimes to factor format
typeCountsFinalData$CloseTimes <- as.factor(typeCountsFinalData$CloseTimes)

ggplot(typeCountsFinalData, aes(TypeCounts, fill = CloseTimes, order = -as.numeric(CloseTimes))) + 
  geom_bar(stat = "bin", position = "fill", alpha=0.6) + 
  scale_fill_brewer(palette = "Blues") + 
  xlab("Types of Services") +
  ylab("CloseTimes") +
  ggtitle("Types of Services and CloseTimes")

# kernel density plot about types of services and duration
# change TypeCounts to factor format
typeCountsFinalData$TypeCounts <- as.factor(typeCountsFinalData$TypeCounts)

ggplot(typeCountsFinalData, aes(TypeCounts, Duration)) + 
  geom_boxplot()

ggplot(typeCountsFinalData, aes(Duration, fill = TypeCounts)) + 
  geom_density(alpha=0.3)+
  scale_fill_brewer(palette = "Blues") + 
  ggtitle("Types of Services and Duration")+
  theme_bw() +
  facet_wrap(~TypeCounts)