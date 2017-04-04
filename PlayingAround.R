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

setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying") # change to where you put DHSTeam1 folder
library(readxl)
dat2 <- read_excel("Data/DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs
library(dplyr)
dat688929 <- filter(dat2, CLIENT_ID == 688929)
dat688929clean <- dat688929[, colSums(is.na(dat688929)) < nrow(dat688929)]
dat688929clean <- data.frame(dat688929clean)
mdat688929clean <- melt(dat688929clean, measure.vars = names(dat688929clean)[-1])
mdat688929clean$value <- paste0("01-", mdat688929clean$value)
mdat688929clean$value <- as.Date(mdat688929clean$value, "%d-%B-%Y")
mdat688929clean$time <- as.POSIXct(mdat688929clean$value)

ggplot(mdat688929clean, aes(value,variable)) + 
  geom_line(size = 2) +
  xlab("") + ylab("") +
  theme_bw()+
  scale_x_datetime(breaks=date_breaks("2 sec"),
                   limits = as.POSIXct(c('2014-08-07 09:03:24','2014-08-07 09:03:29')))
