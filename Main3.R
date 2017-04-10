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
# source("Functions/CalServiceBA.R")
# serviceBAData <- calServiceBA(convertedData)


### no longer in use ###

# concurrencyDataFrame <- read.csv("Data/ConcurrencyDataFrame.csv")
# 
# 
# source("Functions/CalConcurrencyRatio.R")
# concurrencyRatio <- calConcurrencyRatio(concurrencyDataFrame)
# 
# source("Functions/MergePlaceAndService.R")
# finalData <- mergePlaceAndService(placeData, concurrencyRatio)
# 
# 
# library(ggplot2)
# source("Graph/KernelDensityPlot.R")
# generateKernelDensity(finalData, "ACHA","placement")
# generateKernelDensity(finalData, "MH","placement")
# generateKernelDensity(finalData, "DA","placement")
# generateKernelDensity(finalData, "DPW_FS","placement")

