setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
rm(list = ls())

mergedData <- read.csv("Data/MergedData.csv")


placeData <- read.csv("Data/Placement.csv")



source("Functions/CalNumServiceFamily.R")
serviceData <- calNumServiceFamily(mergedData)
# export
# write.csv(serviceData, "Data/NumService.csv")


source("Functions/MergePlaceAndService.R")
finalData <- mergePlaceAndService(placeData, serviceData)


library(ggplot2)
source("Graph/Boxplot.R")
generateBoxPlot(finalData, "KindServiceFamily", "Number of Service Kinds and Child Placement")
g1 <- generateBoxPlot(finalData, "NumHousingFamily", "Number of Housing Service and Child Placement")
g2 <- generateBoxPlot(finalData, "NumBehaviorFamily", "Number of Behavior Service and Child Placement")
g3 <- generateBoxPlot(finalData, "NumNutritionFamily", "Number of Nutrition Service and Child Placement")
g4 <- generateBoxPlot(finalData, "NumMentalFamily", "Number of Mental Service and Child Placement")

source("Graph/Multiplot.R")
multiplot(g1, g2, g3, g4, cols=2)

source("Graph/TFplot.R")
generateTFPlot(finalData$KindServiceFamily,finalData$NumHousingFamily,finalData$NumBehaviorFamily,finalData$NumNutritionFamily,finalData$NumMentalFamily,finalData$placement)     
  
source("Graph/KernelDensityPlot.R")             
generateKernelDensity(finalData, "KindServiceFamily")               
               
               
               