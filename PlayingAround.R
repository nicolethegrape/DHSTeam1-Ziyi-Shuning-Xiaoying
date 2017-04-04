
# example
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

# my own effort
rm(list = ls())
library(readxl)
library(reshape)
library(ggplot2)
library(dplyr)
library(scales)
setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
dat2 <- read_excel("Data/DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs
dat688929 <- filter(dat2, CLIENT_ID == 688929)
dat688929clean <- dat688929[, colSums(is.na(dat688929)) < nrow(dat688929)]
dat688929clean <- data.frame(dat688929clean)
mdat688929clean <- melt(dat688929clean, measure.vars = names(dat688929clean)[-1])
mdat688929clean$value <- paste0("01-", mdat688929clean$value)
mdat688929clean$value <- as.Date(mdat688929clean$value, "%d-%B-%Y")
mdat688929clean$time <- as.POSIXct(mdat688929clean$value)
mdat <- mdat688929clean

idxMin <- regexpr("MIN_ACTIVE", mdat$variable) 
idxMax <- regexpr("MAX_ACTIVE", mdat$variable) 
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
serviceName <- substr(mdat$variable, 1, idx)
mdat <- data.frame(mdat, serviceName = serviceName)
minIdx <- which(idxMin > 0)
maxIdx <- which(idxMax > 0)
mins <- rep("min", dim(mdat)[1])
mdat <- data.frame(mdat, minOrMax = mins, stringsAsFactors=FALSE)
mdat$minOrMax[maxIdx] <- "max"

ggplot(mdat, aes(time,serviceName)) + 
  geom_line(size = 2) +
  xlab("") + ylab("") +
  theme_bw()+
  scale_x_datetime(breaks=date_breaks("1 year"),
                   limits = as.POSIXct(c('2002-06-30 20:00:00','2017-01-31 19:00:00')))
