setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
rm(list = ls())
library(zoo)

mergedData <- read.csv("Data/MergedData.csv")


placeData <- read.csv("Data/Placement.csv")


# source("Functions/KeepOnlyService.R")
# cleanedData <- keepOnlyService(mergedData)
# 
# source("Functions/ConvertDate.R")
# convertedData <- convertDate(cleanedData)
# 
# source("Functions/CalConcurrencyForClient.R")
# concurrencyDataFrame <- calConcurrencyForClient(convertedData)
# 
# write.csv(concurrencyDataFrame, "Data/ConcurrencyDataFrame.csv", row.names=FALSE)


concurrencyDataFrame <- read.csv("Data/ConcurrencyDataFrame.csv")


source("Functions/CalConcurrencyRatio.R")
concurrencyRatio <- calConcurrencyRatio(concurrencyDataFrame)

source("Functions/MergePlaceAndService.R")
finalData <- mergePlaceAndService(placeData, concurrencyRatio)


library(ggplot2)
source("Graph/KernelDensityPlot.R")
generateKernelDensity(finalData, "ACHA")
generateKernelDensity(finalData, "DA")
generateKernelDensity(finalData, "DPW_FS")
generateKernelDensity(finalData, "MH")

