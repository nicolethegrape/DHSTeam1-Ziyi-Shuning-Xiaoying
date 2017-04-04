rm(list = ls())
setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder

library(readxl)
dat1 <- read_excel("Data/DHS_Case_Clients_2016EntryCohort.xlsx", 
                   sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
dat2 <- read_excel("Data/DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs

source("Functions/GetPlacementFromACCEPT_REASON.R")
mergedData <- mergeData(dat1, dat2)
placeData <- getPlaceData(mergedData)

source("Functions/GetPlacementFromCrossSystem.R")
placeData2 <- getPlaceData2(mergedData)
all(placeData$CASE_ID == placeData2$CASE_ID) # TRUE
table(placeData$isPlacedFromAcceptReason) # from ACCEPT_REASON
table(placeData2$isPlacedFromCrossSystem) # from CrossSystem

# make placement TRUE if either of them TRUE
placeDataNew <- placeData %>%
  mutate(isPlacedFromCrossSystem = placeData2$isPlacedFromCrossSystem) %>%
  mutate(isPlacedFromAny= isPlacedFromAcceptReason | isPlacedFromCrossSystem)
table(placeDataNew$isPlacedFromAny)

source("Functions/GetNumberOfServices1.R")
serviceDataNew <- calAverNumServiceFamily(mergedData)

source("Functions/MergeServiceAndPlacement.R")
finalData <- getFinalData(placeDataNew, serviceDataNew)

source("Graph/Boxplot.R")
library(ggplot2)
generateBoxPlot(finalData, "averNumServiceFamily", "Number of Services and Child Placement")
g1 <- generateBoxPlot(finalData, "averNumHousingFamily", "Number of Housing Service Per Person and Child Placement")
g2 <- generateBoxPlot(finalData, "averNumBehaviorFamily", "Number of Behavior Service Per Person and Child Placement")
g3 <- generateBoxPlot(finalData, "averNumNutritionFamily", "Number of Nutrition Service Per Person and Child Placement")
g4 <- generateBoxPlot(finalData, "averNumMentalFamily", "Number of Mental Service Per Person and Child Placement")

source("Graph/Multiplot.R")
multiplot(g1, g2, g3, g4, cols=2)

source("Graph/TFplot.R")
generateTFPlot(finalData$averNumServiceFamily,finalData$averNumHousingFamily,finalData$averNumBehaviorFamily,finalData$averNumNutritionFamily,finalData$averNumMentalFamily,finalData$placement)




