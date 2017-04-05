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
concurrencyRatio <- ConcurrencyRatioData(concurrencyDataFrame)

