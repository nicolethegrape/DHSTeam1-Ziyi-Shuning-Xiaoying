setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
rm(list = ls())
library(zoo)

mergedData <- read.csv("Data/MergedData.csv")


placeData <- read.csv("Data/Placement.csv")


source("Functions/KeepOnlyService.R")
cleanedData <- keepOnlyService(mergedData)

source("Functions/ConvertDate.R")
convertedData <- convertDate(cleanedData)

source("Functions/CalConcurrencyForClient.R")
concurrency <- calConcurrencyForClient(convertedData[1:6, ])

write.csv(concurrencyDataFrame, "Data/Concurrency.csv")

source("Functions/CalConcurrencyRatio.R")
concurrencyRatio <- calConcurrencyRatio(concurrency)

source("Functions/GetPlacementFromACCEPT_REASON.R")
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