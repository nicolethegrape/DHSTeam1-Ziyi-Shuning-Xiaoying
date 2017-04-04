
### example
install.packages("reshape")
library(reshape)
library(ggplot2)
library(scales)

tasks <- c("Task1", "Task2")
dfr <- data.frame(
  name        = factor(tasks, levels = tasks),
  start.date  = c("2014-08-07 09:03:25.815", "2014-08-07 09:03:25.956"),
  end.date    = c("2014-08-07 09:03:28.300", "2014-08-07 09:03:30.409")
)

mdfr <- melt(dfr, measure.vars = c("start.date", "end.date"))

mdfr$time<-as.POSIXct(mdfr$value)

ggplot(mdfr, aes(time,name)) + 
  geom_line(size = 2) +
  xlab("") + ylab("") +
  theme_bw()+
  scale_x_datetime(breaks=date_breaks("2 sec"),
                   limits = as.POSIXct(c('2014-08-07 09:03:24','2014-08-07 09:03:29')))





### my own effort

rm(list = ls())
library(readxl)
library(reshape)
library(ggplot2)
library(dplyr)
library(scales)
setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
dat1 <- read_excel("Data/DHS_Case_Clients_2016EntryCohort.xlsx", 
                   sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
dat2 <- read_excel("Data/DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs

source("Functions/GetPlacementFromACCEPT_REASON.R")
mergedData <- mergeData(dat1, dat2)

serviceData <- mergedData %>%
  select(CLIENT_ID, CASE_ID, ACHA_MIN_ACTIVE, ACHA_MAX_ACTIVE, 
         DA_MIN_ACTIVE, DA_MAX_ACTIVE, DPW_FS_MIN_ACTIVE, DPW_FS_MAX_ACTIVE, 
         MH_MIN_ACTIVE, MH_MAX_ACTIVE) 

for (i in 3:(length(serviceData))){
  serviceData[,i] <- paste0("01-", serviceData[,i])
  serviceData[,i] <- as.Date(serviceData[,i], "%d-%B-%Y")
}

serviceDataNew <- serviceData %>%
  group_by(CASE_ID) %>%
  summarise(ACHA_MIN = as.POSIXct(min(ACHA_MIN_ACTIVE, na.rm = TRUE)),
            ACHA_MAX = as.POSIXct(max(ACHA_MAX_ACTIVE, na.rm = TRUE)),
            DA_MIN = as.POSIXct(min(DA_MIN_ACTIVE, na.rm = TRUE)),
            DA_MAX = as.POSIXct(max(DA_MAX_ACTIVE, na.rm = TRUE)),
            DPW_FS_MIN = as.POSIXct(min(DPW_FS_MIN_ACTIVE, na.rm = TRUE)),
            DPW_FS_MAX = as.POSIXct(max(DPW_FS_MAX_ACTIVE, na.rm = TRUE)),
            MH_MIN = as.POSIXct(min(MH_MIN_ACTIVE, na.rm = TRUE)),
            MH_MAX = as.POSIXct(max(MH_MAX_ACTIVE, na.rm = TRUE)))
serviceDataNew <- data.frame(serviceDataNew)
dat <- melt(serviceDataNew, measure.vars = names(serviceDataNew)[-1])
#####
datclean <- data.frame(datclean)
mdatclean <- melt(datclean, measure.vars = names(datclean)[-1])
mdatclean$value <- paste0("01-", mdatclean$value)
mdatclean$value <- as.Date(mdatclean$value, "%d-%B-%Y")
mdatclean$time <- as.POSIXct(mdatclean$value)
mdat <- mdatclean

dates <- c(as.Date(NA, "%d-%B-%Y"), as.Date(NA, "%d-%B-%Y"), 
           as.Date("01-AUG-2011", "%d-%B-%Y"), 
           as.Date("02-AUG-2011", "%d-%B-%Y"), 
           as.Date("01-JUL-2011", "%d-%B-%Y"))
min(dates, na.rm = TRUE)
#####

idxMin <- regexpr("MIN", dat$variable) 
idxMax <- regexpr("MAX", dat$variable) 
getLargerOnes <- function(a, b){
  print(paste0("Do they have same length? ", (length(a) == length(b))))
  result <- NULL
  for (i in 1:length(a)){
    if (a[i] >= b[i]){
      result <- c(result, a[i])
    } else {
      result <- c(result, b[i])
    }
  }
  return(result)
}
idx <- getLargerOnes(idxMin, idxMax)
idx <- idx - 2
serviceName <- substr(dat$variable, 1, idx)
dat <- data.frame(dat, serviceName = serviceName)
minIdx <- which(idxMin > 0)
maxIdx <- which(idxMax > 0)
mins <- rep("min", dim(dat)[1])
dat <- data.frame(dat, minOrMax = mins, stringsAsFactors=FALSE)
dat$minOrMax[maxIdx] <- "max"

ggplot(dat[1:3098,], aes(value,serviceName, col = serviceName)) + 
  geom_line(size = 2) +
  xlab("") + ylab("") +
  theme_bw()+
  scale_x_datetime(breaks=date_breaks("1 year"),
                   limits = as.POSIXct(c('2002-06-30 20:00:00','2017-01-31 19:00:00'))) +
  facet_grid(CASE_ID~.)
