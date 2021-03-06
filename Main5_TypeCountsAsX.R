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

tc <- read.csv("Data/TypeCountsFinalData.csv")

countCat <- NULL
for (i in 1: dim(typeCountsFinalData)[1]){
  cat <- 0
  if (tc[i, 2] < 4){
    cat <- 1
  } else if (tc[i, 2] < 6){
    cat <- 2
  } else {
    cat <- 3
  }
  countCat <- c(countCat, cat)
}

tc <- cbind.data.frame(tc, countCat)

library(ggplot2)

tc$countCat <- as.factor(tc$countCat)

ggplot(tc, aes(x = countCat, fill = PlacementAsY, order = -as.numeric(PlacementAsY))) + 
  geom_bar(stat = "bin", position = "fill", alpha=0.6, width = 0.5) + 
  xlab("Number of Services") +
  ylab("Percentages") +
  ggtitle("Number of Services and Placement") + 
  scale_fill_discrete(labels=c("No Placement", "Placement")) +
  theme_bw() +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 22, face = "bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.position = "top",
        legend.title=element_blank())

## Temporarily put graph codes here

# bar plot about types of services and placement
library(ggplot2)
typeCountsFinalData$TypeCounts <- as.factor(typeCountsFinalData$TypeCounts)
ggplot(typeCountsFinalData, aes(x = TypeCounts, fill = PlacementAsY, order = -as.numeric(PlacementAsY))) + 
  geom_bar(stat = "bin", position = "fill", alpha=0.6, width = 0.5) + 
  geom_text(aes(label=), position=position_dodge(width=0.6), hjust=0.5, vjust=-1, fontface="bold") + 
  xlab("Number of Services") +
  ylab("Percentages") +
  ggtitle("Number of Services and Placement") + 
  scale_fill_discrete(labels=c("No Placement", "Placement")) +
  theme_bw() +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 22, face = "bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.position = "top",
        legend.title=element_blank())

tcPercent <- tc %>%
  group_by(TypeCounts) %>%
  summarise(PlacePercent = round(length(which(PlacementAsY == TRUE)) / n() * 100, 1))

ggplot(typeCountsPercent, aes(x = TypeCounts, y = PlacePercent)) + 
  geom_point()

t.test(typeCountsPercent$PlacePercent~typeCountsPercent$TypeCounts)


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
  geom_jitter() + 
  geom_smooth(aes(group = 1),method = "lm")

ggplot(typeCountsFinalData, aes(Duration, fill = TypeCounts)) + 
  geom_density(alpha=0.3)+
  scale_fill_brewer(palette = "Blues") + 
  ggtitle("Types of Services and Duration")+
  theme_bw() +
  facet_wrap(~TypeCounts)

