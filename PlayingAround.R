
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
dat <- filter(dat2, CLIENT_ID == 688929 | CLIENT_ID == 688932
              | CLIENT_ID == 688934 | CLIENT_ID == 688935
              | CLIENT_ID == 688936 | CLIENT_ID == 688937)
datclean <- dat[, colSums(is.na(dat)) == 0]
datclean <- data.frame(datclean)
mdatclean <- melt(datclean, measure.vars = names(datclean)[-1])
mdatclean$value <- paste0("01-", mdatclean$value)
mdatclean$value <- as.Date(mdatclean$value, "%d-%B-%Y")
mdatclean$time <- as.POSIXct(mdatclean$value)
mdat <- mdatclean

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
                   limits = as.POSIXct(c('2002-06-30 20:00:00','2017-01-31 19:00:00'))) +
  facet_grid(CLIENT_ID~.)
