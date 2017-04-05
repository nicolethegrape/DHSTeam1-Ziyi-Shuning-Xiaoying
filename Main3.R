rm(list = ls())
setwd("~/Desktop/2017Spring/R/DHSTeam1") # change to where you put DHSTeam1 folder

library(readxl)
dat1 <- read_excel("Data/DHS_Case_Clients_2016EntryCohort.xlsx", 
                   sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
dat2 <- read_excel("Data/DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs

source("Functions/MergeData.R")
mergedData <- mergeData(dat1, dat2)
source("Functions/GetConcurrentServices.R")
newMergedData <- convertDate(mergedData)
concurrencyDataFrame <- calConcurrentForClient(newMergedData)
write.csv(concurrencyDataFrame, "Data/ConcurrencyDataFrame.xlsx")

concurrencyRatio <- ConcurrencyRatioData(concurrencyDataFrame)

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