
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



mergedDat <- read.csv("Data/MergedData.csv")
selectDat <- mergedDat[mergedDat$CASE_ID == 37967 | mergedDat$CASE_ID == 59282,]

mergedDat <- read.csv("Data/MergedData.csv")
test <- mergedDat[mergedDat$CASE_ID %in% typeCounts[which(typeCounts$TypeCounts == 7 | typeCounts$TypeCounts == 8),1],]

serviceBAData <- read.csv("Data/ServiceBAData.csv")
library(dplyr)
test2 <- serviceBAData %>%
  select(CLIENT_ID, CASE_ID, MH) %>%
  group_by(CASE_ID) %>%
  summarise(MHpercent = length(which(MH == -1)) / n())


# mean(mhpercent) = 0.4334491
# mean(mhpercent) for families with 8 type counts = 
# mean(mhpercent) for families with 7 type counts = 
# mean(mhpercent) for families with 6 type counts = 
0.53
0.51
0.57
0.49
0.51
0.50
0.46
0.40
0.29
typeCounts <- typeCountsFinalData
calMeans <- function(test2, typeCounts){
  means <- NULL
  for (i in 0:8){
    case_ids <- typeCounts[which(typeCounts$TypeCounts == i), 1]
    mhs <- data.frame(test2[test2$CASE_ID %in% case_ids, 2])
    means <- c(means, round(mean(mhs$MHpercent) * 100, 1))
  }
  return(means)
}
means <- calMeans(test2, typeCounts)
counts <- seq(0:8) - 1
meandf <- cbind.data.frame(counts, means)

ggplot(meandf, aes(x = counts, y = means)) + 
  geom_point()

table(typeCounts$TypeCounts)

case_ids <- typeCounts[which(typeCounts$TypeCounts == 6), 1]
mergedData <- read.csv("Data/MergedData.csv")
filtered <- mergedData %>%
  filter(CASE_ID %in% case_ids)


finalDat <- read.csv("Data/FamilyFinalData.csv")
typeCounts <- read.csv("Data/CompleteXYData.csv")

install.packages("plotrix")
install.packages("reshape")
install.packages("data.table")
install.packages("RColorBrewer")
library(plotrix)
library(reshape)
library(data.table)
library(RColorBrewer)

x <- table(typeCounts$TypeCounts)
piepercent <- round(100*x/sum(x), 1) 
piepercent <-paste(piepercent, "%", sep = "")
par(mfrow=c(1,1),mar=c(2,1.5,1,0.5),oma=c(2,1.5,1,0.5))
pie(x, labels = piepercent, radius = 0.8,main="Pie Chart of Service Type Count", col=brewer.pal(9,"Blues"), clockwise = FALSE,
    density = NULL, angle = 45, lty = NULL, border = NULL, edges = 200)
legend("bottom", legend=c("0", "1", "2", "3", "4", "5", "6", "7", "8"), 
       cex = 0.6, horiz = T, fill =brewer.pal(9,"Blues") )



