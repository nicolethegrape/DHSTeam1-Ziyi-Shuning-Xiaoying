setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
rm(list = ls())
library(readxl)
dat1 <- read_excel("Data/DHS_Case_Clients_2016EntryCohort.xlsx", 
                   sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
dat2 <- read_excel("Data/DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs

source("Functions/MergeData.R")
mergedData <- mergeData(dat1, dat2)
# write.csv(mergedData, "Data/MergedData.csv", row.names=FALSE)



source("Functions/GetPlaceFromACCEPT_REASON.R")
placeData1 <- getPlaceFromACCEPT_REASON(mergedData)

source("Functions/GetPlaceFromCrossSystem.R")
placeData2 <- getPlaceFromCrossSystem(mergedData)

# check two place data
all(placeData1$CASE_ID == placeData2$CASE_ID) # TRUE
table(placeData1$isPlacedFromAcceptReason) # from ACCEPT_REASON
table(placeData2$isPlacedFromCrossSystem) # from CrossSystem

source("Functions/MergePlace.R")
placeData <- mergePlace(placeData1, placeData2)

# check final place data
table(placeData$isPlacedFromAny)
# export
# write.csv(placeData, "Data/Placement.csv", row.names=FALSE)



source("Functions/CalAverNumServiceFamily.R")
serviceData <- calAverNumServiceFamily(mergedData)
# export
# write.csv(serviceData, "Data/AverNumService.csv", row.names=FALSE)



source("Functions/MergePlaceAndService.R")
finalData <- mergePlaceAndService(placeData, serviceData)


library(ggplot2)
source("Graph/Boxplot.R")
generateBoxPlot(finalData, "averNumServiceFamily", "Number of Services and Child Placement")
g1 <- generateBoxPlot(finalData, "averNumHousingFamily", "Number of Housing Service Per Person and Child Placement")
g2 <- generateBoxPlot(finalData, "averNumBehaviorFamily", "Number of Behavior Service Per Person and Child Placement")
g3 <- generateBoxPlot(finalData, "averNumNutritionFamily", "Number of Nutrition Service Per Person and Child Placement")
g4 <- generateBoxPlot(finalData, "averNumMentalFamily", "Number of Mental Service Per Person and Child Placement")

source("Graph/Multiplot.R")
multiplot(g1, g2, g3, g4, cols=2)

source("Graph/TFplot.R")
generateTFPlot(finalData$averNumServiceFamily,finalData$averNumHousingFamily,finalData$averNumBehaviorFamily,finalData$averNumNutritionFamily,finalData$averNumMentalFamily,finalData$placement)

source("Graph/KernelDensityPlot.R")
generateKernelDensity(finalData, "averNumServiceFamily")


