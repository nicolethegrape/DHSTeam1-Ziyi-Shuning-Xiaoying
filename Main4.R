setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
rm(list = ls())
library(zoo)

# mergedData <- read.csv("Data/MergedData.csv")
# 
# source("Functions/KeepOnlyService.R")
# cleanedData <- keepOnlyService(mergedData)
# 
# source("Functions/ConvertDate.R")
# convertedData <- convertDate(cleanedData)

# source("Functions/CalServiceBA.R")
# serviceBAData <- calServiceBA(convertedData)
# 
# write.csv(serviceBAData, "Data/ServiceBAData.csv", row.names=FALSE)

serviceBAData <- read.csv("Data/ServiceBAData.csv")

source("Functions/CalFamilyPreCYF.R")
familyPreCYFData <- calFamilyPreCYF(serviceBAData)

source("Functions/AggrFamilyPreCYFCat.R")
familyPreCYFCatData <- aggrFamilyPreCYFCat(familyPreCYFData)

write.csv(familyPreCYFCatData, "Data/FamilyPreCYFCatData.csv", row.names=FALSE)

source("Functions/CalFamilyPlacePostCYF.R")
familyPlacePostCYFData <- calFamilyPlacePostCYF(serviceBAData)

write.csv(familyPlacePostCYFData, "Data/FamilyPlacePostCYFData.csv", row.names=FALSE)


durationAndCloseTimes <- read.csv("Data/DurationAndCloseTimes.csv")

source("Functions/MergeXYVars.R")
familyFinalData <- mergeXYVars(familyPreCYFCatData, familyPlacePostCYFData, durationAndCloseTimes)

write.csv(familyFinalData, "Data/FamilyFinalData.csv", row.names=FALSE)






