setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
rm(list = ls())
library(readxl)
dat1 <- read_excel("Data/DHS_Case_Clients_2016EntryCohort.xlsx", 
                   sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
dat2 <- read_excel("Data/DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs

source("Functions/MergeData.R")
mergedData <- mergeData(dat1, dat2)

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
write.csv(placeData, "Data/Placement.xlsx")