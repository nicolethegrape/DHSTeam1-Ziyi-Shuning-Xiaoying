rm(list = ls())
setwd("~/Desktop/2017Spring/R/DHSTeam1/TEAM1FinalCode")

# library all the packages
library(dplyr)
library(readr)
library(readxl)
library(zoo)
library(ggplot2)
library(plyr)
library(magrittr)
library("stringi")
install.packages("plotrix")
install.packages("readr")
install.packages("RColorBrewer")
library(plotrix)
library(readr)
library(RColorBrewer)


###### DHS Code####

####################Part1########################
################Data cleaning ###################

# generate merged data --- Shuning 
lengthunique<- function(x) {
  length(unique(x))
} 
latest <- function(x){
  if (any(is.na(x))){
    return(Sys.Date())
  }
  reformatted <- paste0("01-", x)
  reformattedDate <- as.Date(reformatted, "%d-%B-%Y")
  return(max(reformattedDate))
}

mergeData <- function(entrycohort, crosssystem){
  listOfClient <- sort(unique(entrycohort$CLIENT_ID))
  ncase <- tapply(entrycohort$CASE_ID, entrycohort$CLIENT_ID, lengthunique)
  multiCaseId <- which(ncase > 1)
  multiCaseClient <- listOfClient[multiCaseId]
  multiCaseIdCohort <- which(entrycohort$CLIENT_ID %in% multiCaseClient) # matches any of multiCaseClient
  entrycohort <- entrycohort[-multiCaseIdCohort,]
  lastClose <- structure(tapply(entrycohort$CLOSE_DT, entrycohort$CLIENT_ID, latest), class = "Date")
  entrycohortUnique <- entrycohort[!duplicated(entrycohort$CLIENT_ID),] 
  entrycohortUnique <- entrycohortUnique[,-c(14, 15)] 
  entrycohortUnique <- arrange(entrycohortUnique, CLIENT_ID)
  entrycohortUnique <- data.frame(entrycohortUnique, LAST_CLOSE_DT = lastClose)
  mergedData <- merge(x = entrycohortUnique, y = crosssystem, by = "CLIENT_ID") # inner join
  return(mergedData)
}

dat1 <- read_excel("DHS_Case_Clients_2016EntryCohort.xlsx", 
                   sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
dat2 <- read_excel("DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs
mergedData <- mergeData(dat1, dat2)

# drop unused data, only keep CLIENT_ID, CASE_ID, CL_INLV_START, LAST_CLOSE_DT, ACHA_MIN_ACTIVE:ID_MAX_ACTIVE   
# ---Shuning 
keepOnlyService <- function(mergedData){
  mergedData <- mergedData %>% 
    select(c(CLIENT_ID:CASE_ID, SRVC_ACPT_DT_MY, ACHA_MIN_ACTIVE:ID_MAX_ACTIVE))   
}
cleanedData <- keepOnlyService(mergedData)

# Convert the date of the data --- Xiaoying 
convertDate<-function(mergedData){
  library(zoo)
  DateDat<-mergedData
  for(i in c(3:45)){
    DateDat[,i]<-as.Date(as.yearmon(DateDat[,i],"%b-%Y"))
  }
  return(DateDat)
}
convertedData <- convertDate(cleanedData)

# generate the service Before/After data for the client level -- Ziyi 

calServiceBA <- function(newMergedData){
  rows <- dim(newMergedData)[1]
  cols <- dim(newMergedData)[2]
  
  idxMax <- regexpr("MAX", names(newMergedData)[seq(5, length(newMergedData), by = 2)]) 
  idxMax <- idxMax - 2
  serviceNames <- substr(names(newMergedData)[seq(5, length(newMergedData), by = 2)], 1, idxMax)
  l <- length(serviceNames)
  concurrencyDataFrame <- data.frame(matrix(ncol = l + 2, nrow = 0))
  colnames(concurrencyDataFrame) <- c("CLIENT_ID", "CASE_ID", serviceNames)
  
  for (i in 1:rows){
    client_id = newMergedData[i, 1]
    case_id = newMergedData[i, 2]
    A <- as.Date(newMergedData[i, 3])
    concurrencyVector <- c(client_id, case_id)
    for (j in seq(5,cols,by = 2)){
      MAX <- as.Date(newMergedData[i, j])
      concurrency <- NA
      
      if (is.na(MAX)) {
        concurrency<-0
      } else if (MAX>=A) {
        concurrency <- 1
      } else if (MAX<A) {
        concurrency<--1
      }
      concurrencyVector[length(concurrencyVector) + 1] <- concurrency
    }
    concurrencyDataFrame[nrow(concurrencyDataFrame)+1,] <- concurrencyVector
  }
  return(concurrencyDataFrame)
}
ServiceBAData<-calServiceBA(convertedData)
# write the serviceBAData for future use
write.csv(ServiceBAData, "ServiceBAData.csv", row.names=FALSE)

#generate the family level final data -- Shuning
# convert client-level-before-after-data into family-level-before-data
preCYFIsTrue <- function(x){
  if (any(x == -1)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
library(dplyr)
calFamilyPreCYF <- function(serviceBAData){
  familyPreCYFData <- serviceBAData %>%
    group_by(CASE_ID) %>%
    summarise_each(funs(preCYFIsTrue), -CLIENT_ID)
  return(familyPreCYFData)
}

familyPreCYFData <- calFamilyPreCYF(ServiceBAData)
# write.csv(familyPreCYFData, "FamilyPreCYFData.csv", row.names=FALSE)

# aggregate the family PRE-CYF Catagories  --- Shuning 
aggrFamilyPreCYFCat <- function(familyPreCYFData){
  names <- c("CASE_ID", "Placement", "Housing", "BasicNeeds", "FSC")
  placementTrue <- Reduce("|", select(familyPreCYFData, CYF_KPL, CYF_PL_O))
  housingTrue <- Reduce("|", select(familyPreCYFData, ACHA, HH, HACP))
  basicNeedsTrue <-  Reduce("|", select(familyPreCYFData, DPW_FS, DPW_GA, DPW_SSI, DPW_TANF))
  familyPreCYFAggrData <- cbind.data.frame(familyPreCYFData$CASE_ID, placementTrue, housingTrue, basicNeedsTrue, familyPreCYFData$FSC)
  colnames(familyPreCYFAggrData) <- names
  return(familyPreCYFAggrData)
}
familyPreCYFCatData <- aggrFamilyPreCYFCat(familyPreCYFData)
# write.csv(familyPreCYFCatData, "FamilyPreCYFCatData.csv", row.names=FALSE)

# convert client-level-before-after-data into family-level-after-data for placement
## Shuning 
postCYFIsTrue <- function(x){
  if (any(x == 1)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

calFamilyPlacePostCYF <- function(serviceBAData){
  familyPostCYFData <- serviceBAData %>%
    group_by(CASE_ID) %>%
    summarise(CYF_KPL = postCYFIsTrue(CYF_KPL), CYF_PL_O = postCYFIsTrue(CYF_PL_O))
  
  placementTrue <- Reduce("|", select(familyPostCYFData, CYF_KPL, CYF_PL_O))
  familyPlacePostCYFData <- cbind.data.frame(familyPostCYFData$CASE_ID, placementTrue)
  colnames(familyPlacePostCYFData) <- c("CASE_ID", "Placement")
  return(familyPlacePostCYFData)
}

familyPlacePostCYFData <- calFamilyPlacePostCYF(ServiceBAData)
# write.csv(familyPlacePostCYFData, "FamilyPlacePostCYFData.csv", row.names=FALSE)

##################################################################
# get durationAndCloseTimes -----Group2 Jia
###################################################################
clientdat<-read.csv("ServiceBAData.csv")
library("dplyr")
library("stringi")

# Task1: Aggreagate Client level-->Case level. 
# count as -1 if before, 1 if any after, 0 if all NA

# Function groupvalue: create variable that assign value1 if any value1, else: value2 if any value2, else: value 3. 
groupvalue<-function(var,value1=-1,value2=1,value3=0){
  output<-ifelse(any(var==value1), value1,ifelse(any(var==value2),value2,value3))
}

case_group<-group_by(clientdat,CASE_ID)
casedat<-summarise_each(case_group,funs(groupvalue))
print(casedat)
casedat<-select(casedat,-(CLIENT_ID))

#### Task2: close times
dat<-read.csv("ShortenClientsMerged.csv")

nClosedate<-function(closedates){
  a<-stri_count_fixed(closedates, ",")+1
  a[which(is.na(a))]<-1
  output<-a
}
# Client level
dat$nClose<-nClosedate(dat$CloseDate)
clientdat<-cbind(clientdat,dat$nClose)
# Case level
case_group<-group_by(dat,CaseID)
dat2<-summarise(case_group,nClose=max(nClose)) # caseID and nClose table
# table(dat2$nClose)

### Task3: duration
# 1) last close date
class(dat$CloseDate)
# length(which(is.na(dat$CloseDate))) is 2046
s_close<-strsplit(as.character(dat$CloseDate),split=",")
s_close<-sapply(s_close,sort)
s_close[which(s_close=="character(0)")]<-"NA" # 2046 characters have been changed

dat$lastClose<-sapply(s_close, tail, 1)
# table(dat$lastClose)
dat$lastClose[dat$lastClose=="NA"]<-"2017-02-01"
# 2) Client level duration
dat$AcceptDate<-as.Date(dat$AcceptDate)
dat$lastClose<-as.Date(dat$lastClose)
dat$duration=dat$lastClose-dat$AcceptDate
# table(dat$duration)
clientdat<-cbind(clientdat,dat$duration)
# 3) Case level duration
case_group<-group_by(dat,CaseID)
dat2<-summarise(case_group,nClose=max(nClose),Duration=max(duration))
write.csv(dat2, "DurationAndCloseTimes.csv", row.names=FALSE)
FamilyDataDeep<-merge(casedat,dat2,by.x="CASE_ID", by.y="CaseID")
write.csv(FamilyDataDeep,"FamilyDataDeep.csv")

durationAndCloseTimes <- read.csv("DurationAndCloseTimes.csv")


## Extra Duration without NA

# 1) last close date
class(dat$CloseDate)
# length(which(is.na(dat$CloseDate))) is 2046
s_close<-strsplit(as.character(dat$CloseDate),split=",")
s_close<-sapply(s_close,sort)
s_close[which(s_close=="character(0)")]<-"NA" # 2046 characters have been changed

dat$lastClose<-sapply(s_close, tail, 1)
# table(dat$lastClose)
dat$lastClose[dat$lastClose=="NA"]<-"2017-03-01"
table(dat$lastClose)
# 2) Client level duration
dat$AcceptDate<-as.Date(dat$AcceptDate)
dat$lastClose<-as.Date(dat$lastClose)
dat$duration2=dat$lastClose-dat$AcceptDate

dat$duration2[which(dat$lastClose=="2017-03-01")]<-NA
clientdat<-cbind(clientdat,dat$duration2)
write.csv(clientdat,"ClientLeveldata.csv")

#case level
case_group<-group_by(dat,CaseID)
dat2<-summarise(case_group,nClose=max(nClose),Duration2=max(duration))
FamilyDataDeep<-cbind(FamilyDataDeep,duration2=dat2$Duration2)
FamilyDataDeep<-select(FamilyDataDeep,-(10:11))
FamilyDataDeep<-rename(FamilyDataDeep,duration2=dat2.Duration)

#head(FamilyFinalData)
write.csv(FamilyDataDeep,"FamilyDataDeep.csv",row.names = FALSE)



########Close time and Duration End ###########################

# merge the X and Y variables  --- Shuning 
mergeXYVars <- function(xVars, placeAsY, durationAndCloseTimes){
  familyFinalData <- cbind.data.frame(xVars, placeAsY[,2], durationAndCloseTimes[,2:3])
  colnames(familyFinalData) <- c(colnames(xVars), "PlacementAsY", "CloseTimes", "Duration")
  return(familyFinalData)
}

familyFinalData <- mergeXYVars(familyPreCYFCatData, familyPlacePostCYFData, durationAndCloseTimes)

# calculate type counts function ---Shuning 
calTypeCounts <- function(familyPreCYFData){
  test <- familyPreCYFData %>%
    select(CASE_ID:ACHA, DPW_FS:DPW_TANF, FSC:HH)
  typeCounts <- NULL
  for (i in 1:dim(test)[1]){
    typeCounts <- c(typeCounts, length(which(test[i,-1] == TRUE)))
  }
  typeCountsDataFrame <- data.frame(test$CASE_ID, typeCounts)
  colnames(typeCountsDataFrame) <- c("CASE_ID", "TypeCounts")
  return(typeCountsDataFrame)
}

typeCountsData <- calTypeCounts(familyPreCYFData)
typeCountsFinalData <- mergeXYVars(typeCountsData, familyPlacePostCYFData, durationAndCloseTimes)
write.csv(typeCountsFinalData, "typeCountsFinalData.csv", row.names=FALSE)

# get the final complete data

xVars <- cbind.data.frame(familyPreCYFCatData, typeCountsData)
CompleteXYData <- mergeXYVars(xVars, familyPlacePostCYFData, durationAndCloseTimes)
write.csv(CompleteXYData, "CompleteXYData.csv", row.names=FALSE)


###############Group 2 Deeper Research  --Jia###############
####### Family Size and number of children #################
raw<-read.csv("ShortenClientsMerged.csv")

## Family size
PeopleinCase<-function(datasetname,groupname){
  datasetname<-group_by_(datasetname,groupname)
  newdata<-summarise(datasetname, nClients=n())
}

nFamily<-PeopleinCase(raw,"CaseID")
deepData<-merge(FamilyDataDeep,nFamily,by.x="CASE_ID",by.y="CaseID",all = TRUE)

## number of Children
nChild<-raw %>%
  group_by(CaseID)%>%
  summarize(nChildren=length(CrossID[Role=="C"]))
deepData<-merge(FamilyDataDeep,nChild,by.x="CASE_ID",by.y="CaseID")

write.csv(deepData,"deepData.csv")

###0418# Dig deeper
#36
table(casedat$DPW_FS)
table(casedat$DPW_GA)
table(casedat$DPW_SSI)
table(casedat$DPW_TANF)

a<-filter(casedat,DPW_TANF==-1)
length(which(a$HH==-1)) # 122
length(which(a$HACP==-1)) #67
length(which(a$ACHA==-1)) #184
length(which(a$DPW_FS==-1)) #191
length(which(a$DPW_GA==-1)) #317
length(which(a$DPW_SSI==-1)) #476
length(which(a$FSC==-1)) #250

####### Statistics
mean(deepData$Duration[which(deepData$DPW_FS==1)])
mean(deepData$CloseTimes[which(deepData$DPW_FS==1)])

group_by(deepData, BasicNeeds) %>%
  summarise(percent = round(length(which(PlacementAsY == TRUE)) / n() * 100, 1))

mean(deepData$nClients[which(deepData$DPW_FS==1)])

t.test(deepData$Duration~deepData$DPW_FS)
t.test(deepData$CloseTimes~deepData$DPW_FS)
t.test(deepData$PlacementAsY~deepData$DPW_TANF)

##### End #######


####################Part2########################
#####################ggPlot#####################
################################################3


## Graph1##
##Duration Days Count##-Zhehan Andrew
DurationDF <- as.data.frame(table(familyFinalData$Duration))
colnames(DurationDF) <- c("Duration", "Count")

ggplot(DurationDF, aes(Duration, Count)) +
  geom_bar(position="dodge", stat="identity", fill="#708090") +
  geom_label(aes(label=Count), color="#2F4F4F", size=4) + 
  ggtitle("Duration Days Count") +
  geom_vline(aes(xintercept=14.9), colour="red", linetype=5, size=1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


## Graph2##
##Close Time Count##----Duo
CloseTimesDF <- as.data.frame(table(familyFinalData$CloseTimes))
colnames(CloseTimesDF) <- c("CloseTimes", "Count")

ggplot(CloseTimesDF, aes(CloseTimes, Count)) +
  geom_bar(position="dodge", stat="identity", fill="#66CDAA") +
  geom_label(aes(label=Count), color="#008080", size=4) + 
  ggtitle("Close Times Count") +
  geom_vline(aes(xintercept=1.691414), colour="red", linetype=5, size=1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) # Move the title to the center

## Graph3 ##
##Service Type Combot##----Zhehan

onlyHousing <- length(which(familyFinalData$Housing==T & familyFinalData$BasicNeeds==F & familyFinalData$FSC==F))
onlyBN <- length(which(familyFinalData$Housing==F & familyFinalData$BasicNeeds==T & familyFinalData$FSC==F))
onlyFSC <- length(which(familyFinalData$Housing==F & familyFinalData$BasicNeeds==F & familyFinalData$FSC==T))
Housing_BN <- length(which(familyFinalData$Housing==T & familyFinalData$BasicNeeds==T & familyFinalData$FSC==F))
Housing_FSC <- length(which(familyFinalData$Housing==T & familyFinalData$BasicNeeds==F & familyFinalData$FSC==T))
BN_FSC <- length(which(familyFinalData$Housing==F & familyFinalData$BasicNeeds==T & familyFinalData$FSC==F))
Housing_BN_FSC <- length(which(familyFinalData$Housing==T & familyFinalData$BasicNeeds==T & familyFinalData$FSC==T))

Type <- c("only Housing", "only BN", "only FSC", "Housing & BN", "Housing & FSC", "BN & FSC", "Housing & BN & FSC")
Count <- c(onlyHousing, onlyBN, onlyFSC, Housing_BN, Housing_FSC, BN_FSC, Housing_BN_FSC)
serviceTypeCount <- data.frame(Type, Count)

serviceTypeCount$Type <- factor(serviceTypeCount$Type, levels = unique(serviceTypeCount$Type)) #keep the order of types

ggplot(serviceTypeCount, aes(Type, Count)) + 
  geom_bar(position="dodge", stat="identity", fill="#ADD8E6") +
  geom_label(aes(label=Count), color="#000080", size=4) + 
  ggtitle("Service Type Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 


## Graph 4 ##
##Service Type Counts##----Duo

#Distribution of service Type Counts on family level
library(plotrix)
library(readr)
library(RColorBrewer)

dat <- mergedData[,c(grep("CASE_ID", colnames(mergedData)),
                     grep("ACHA_MAX_ACTIVE", colnames(mergedData)),
                     grep("HH_MAX_ACTIVE", colnames(mergedData)),
                     grep("HACP_MAX_ACTIVE", colnames(mergedData)),
                     grep("DPW_FS_MAX_ACTIVE", colnames(mergedData)),
                     grep("DPW_GA_MAX_ACTIVE", colnames(mergedData)),
                     grep("DPW_SSI_MAX_ACTIVE", colnames(mergedData)),
                     grep("DPW_TANF_MAX_ACTIVE", colnames(mergedData)),
                     grep("FSC_MAX_ACTIVE", colnames(mergedData)))]
caseID <- unique(dat$CASE_ID)

numOfType <- NULL
i <- 1
# For each caseID, count how many service columns with entire NA
for (c in caseID){
  df <- dat[which(dat$CASE_ID==c), ]
  x <- 0   # x: how many columns with entire NA
  for (j in 2:9){ #col2 to col9: columns of the 8 services
    if(all(is.na(df[,j]))){ # check if this column is with entire NA
      x <- x+1
    }
  }
  y <- 8-x # y : how many types of services does this caseID have
  numOfType[i] <- y
  i <- i+1
}

countType <- data.frame(caseID, numOfType)

x <- table(countType$numOfType)
piepercent <- round(100*x/sum(x), 1) 
piepercent <-paste(piepercent, "%", sep = "")
par(mfrow=c(1,1),mar=c(2,1.5,1,0.5),oma=c(2,1.5,1,0.5))
pie(x, labels = piepercent, radius = 0.8,main="Pie Chart of Service Type Count", col=brewer.pal(9,"Blues"), clockwise = FALSE,
    density = NULL, angle = 45, lty = NULL, border = NULL, edges = 200)
legend("bottom", legend=c("0", "1", "2", "3", "4", "5", "6", "7", "8"), 
       cex = 0.6, horiz = T, fill =brewer.pal(9,"Blues") )



## Graph 5##
####service and percentage of placement for the three catagories of the services ####
#---------Xiaoying 
# get the plot data 
#generate a percent data frame.
dat <- familyFinalData
GeneratePercent<-function(dat){
  percentTrue<-NULL
  percentFalse<-NULL
  serviceName<-c("Housing","Basic Needs","FSC","Housing","Basic Needs","FSC")
  serviceStatus<-c(rep("TRUE",3),rep("FALSE",3))
  
  for(i in 3:5){
    percentTrue<-c(round(length(which(dat[,i]=="TRUE"&dat[,6]=="TRUE"))/length(which(dat[,i]=="TRUE"))*100, 1),percentTrue)
    percentFalse<-c(round(length(which(dat[,i]=="FALSE"&dat[,6]=="TRUE"))/length(which(dat[,i]=="FALSE"))*100, 1),percentFalse)
  }
  percent<-c(percentTrue,percentFalse)
  plotdata<-data.frame(serviceName,serviceStatus,percent)
  
  return(plotdata)
}

#call function, get new data set.
plotData<-GeneratePercent(dat)

# ggplot 
plotData$serviceName <- as.factor(plotData$serviceName)
positions <- c("Housing", "Basic Needs", "FSC")

ggplot(plotData,aes(y = percent, ymax = max(percent)*1.10, x = serviceName, fill = serviceStatus)) +
  geom_bar(stat = "identity",position = "dodge",alpha = 0.6, width = 0.5) +
  geom_text(aes(label = percent), position = position_dodge(width=0.5), hjust=0.4, vjust=-0.5, fontface = "bold") + 
  ylab("% of Placement") +
  xlab("") +
  ggtitle("Services and Placement") +
  scale_fill_discrete(name="Service Status") +
  scale_x_discrete(limits = positions) +
  theme_bw() +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 22, face = "bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=14,face="bold"),
        legend.position = "top")

## Graph 6 ##
##service and percentage of placement conditional bar chart -- Ziyi ####

# get the plot data -- Shuning 
genFourGroups <- function(x, y){
  groups <- NULL
  len <- length(x)
  for (i in 1:len){
    group <- ""
    if (x[i] == FALSE & y[i] == FALSE){
      group <- "No Housing, No Basic Needs"
    } else if (x[i] == TRUE & y[i] == FALSE){
      group <- "Yes Housing, No Basic Needs"
    } else if(x[i] == FALSE & y[i] == TRUE){
      group <- "No Housing, Yes Basic Needs"
    } else {
      group <- "Yes Housing, Yes Basic Needs"
    }
    groups <- c(groups, group)
  }
  return(groups)
}

groups <- genFourGroups(familyFinalData$Housing, familyFinalData$BasicNeeds)
conditional <- cbind.data.frame(familyFinalData, groups = groups)

ConditionalData <- conditional %>%
  group_by(groups, FSC) %>%
  summarise(percent = round(length(which(PlacementAsY == TRUE)) / n() * 100, 1))

# ggplot -- Ziyi 
ggplot(ConditionalData,aes(y=percent,ymax=max(percent)*1.10, x=groups, fill=FSC))+
  geom_bar(stat = "identity",position = "dodge",alpha=0.7, width = 0.5)  +
  geom_text(aes(label=percent), position=position_dodge(width=0.6), hjust=0.5, vjust=-1, fontface="bold") + 
  scale_x_discrete(
    labels=function(x) {
      sub(",",",\n",x,fixed=TRUE)
    }) +
  ylab("% of Placement") +
  xlab("")+ 
  ggtitle("Conditional Services and Placement") +
  scale_fill_discrete(labels=c("No FSC", "FSC")) +
  theme_bw() +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 22, face = "bold",hjust = 0.5),
        legend.text=element_text(size=12,face="bold"),
        legend.position = "top",
        legend.title=element_blank())

## Graph 7 ##
###bar chart of type counts and percentage of placement -- Shuning 

typeCountsFinalData$TypeCounts <- as.factor(typeCountsFinalData$TypeCounts)

ggplot(typeCountsFinalData, aes(x = TypeCounts, fill = PlacementAsY, order = -as.numeric(PlacementAsY))) + 
  geom_bar(stat = "count", position = "fill", alpha=0.6, width = 0.5) + 
  xlab("Number of Services") +
  ylab("Percentages") +
  ggtitle("Number of Services and Placement") + 
  scale_fill_discrete(labels=c("No Placement", "Placement")) +
  theme_bw() +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 22, face = "bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.position = "top",
        legend.title=element_blank())
## 
#######Three graphs of closetimes#######Alberto########
##
library(readr)
library(ggplot2)
library(dplyr)
library(plyr)

mu <- ddply(familyFinalData, "Housing", summarise, grp.mean=mean(CloseTimes))

ggplot(familyFinalData, aes(x=CloseTimes, fill=Housing,color=Housing)) + 
  geom_density(aes(y = ..density..),binwidth=.5, position="dodge", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Housing),
             linetype="dashed") +
  stat_function(fun = dnorm, colour = "Black",
                args = list(mean = mean(familyFinalData$CloseTimes, na.rm = TRUE),
                            sd = sd(familyFinalData$CloseTimes, na.rm = TRUE)))+
  scale_fill_discrete(breaks=c("FALSE", "TRUE"),
                      labels=c("Post-CYF Service & NA", "Pre-CYF Service")) +
  guides(color=FALSE)

ggplot(familyFinalData, aes(x=Housing, y = CloseTimes, color=Housing)) + 
  geom_boxplot() +
  guides(color=FALSE) +
  theme_bw()



##
mu <- ddply(familyFinalData, "BasicNeeds", summarise, grp.mean=mean(CloseTimes))

ggplot(familyFinalData, aes(x=CloseTimes, fill=BasicNeeds,color=BasicNeeds)) + 
  geom_histogram(aes(y = ..density..),binwidth=.5, position="dodge", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=BasicNeeds),
             linetype="dashed") +
  stat_function(fun = dnorm, colour = "Black",
                args = list(mean = mean(familyFinalData$CloseTimes, na.rm = TRUE),
                            sd = sd(familyFinalData$CloseTimes, na.rm = TRUE)))+
  scale_fill_discrete(breaks=c("FALSE", "TRUE"),
                      labels=c("Post-CYF Service & NA", "Pre-CYF Service")) +
  guides(color=FALSE)


ggplot(familyFinalData, aes(x=BasicNeeds, y = CloseTimes, color=BasicNeeds)) + 
  geom_boxplot() +
  guides(color=FALSE) +
  theme_bw()


##
mu <- ddply(familyFinalData, "FSC", summarise, grp.mean=mean(CloseTimes))

ggplot(familyFinalData, aes(x=CloseTimes, fill=FSC,color=FSC)) + 
  geom_histogram(aes(y = ..density..),binwidth=.5, position="dodge", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=FSC),
             linetype="dashed") +
  stat_function(fun = dnorm, colour = "Black",
                args = list(mean = mean(familyFinalData$CloseTimes, na.rm = TRUE),
                            sd = sd(familyFinalData$CloseTimes, na.rm = TRUE)))+
  scale_fill_discrete(breaks=c("FALSE", "TRUE"),
                      labels=c("Post-CYF Service & NA", "Pre-CYF Service")) +
  guides(color=FALSE)

ggplot(familyFinalData, aes(x=FSC, y = CloseTimes, color=FSC)) + 
  geom_boxplot() +
  guides(color=FALSE) +
  theme_bw()

############Three graphs of duration######Xiaoya##########
###
newdat<-group_by(familyFinalData,Housing)
housingmean<-summarise(newdat,housingmean=mean(Duration))

ggplot(familyFinalData, aes(x=Duration,color=Housing)) + 
  geom_density(size=1)+
  geom_vline(data=housingmean,aes(xintercept = housingmean,color=Housing),linetype="dashed",size=1)+
  geom_vline(xintercept = 147.102,size=1,color="blue")+
  annotate("text", x=170, y=0.0045, label="16",size=5)+
  theme_bw()+
  theme(legend.position = "top")+
  theme(legend.title=element_blank())+
  ggtitle("Housing")+
  theme(plot.title = element_text(size=22))


###
newdat1<-group_by(familyFinalData,BasicNeeds)
basicneedsmean<-summarise(newdat1,basicneedsmean=mean(Duration))

ggplot(familyFinalData, aes(x=Duration,color=BasicNeeds)) + 
  geom_density(size=1)+
  geom_vline(data=basicneedsmean,aes(xintercept = basicneedsmean,color=BasicNeeds),linetype="dashed",size=1)+
  geom_vline(xintercept = 147.102,size=1,color="blue")+
  annotate("text", x=170, y=0.0045, label="18",size=5)+
  theme_bw()+
  theme(legend.position = "top")+
  ylim(0.000,0.005)+
  theme(legend.title=element_blank())+
  ggtitle("Basic Needs")+
  theme(plot.title = element_text(size=22))


###
newdat2<-group_by(familyFinalData,FSC)
FSCmean<-summarise(newdat2,FSCmean=mean(Duration))

ggplot(familyFinalData, aes(x=Duration,color=FSC)) + 
  geom_density(size=1)+
  geom_vline(data=FSCmean,aes(xintercept = FSCmean,color=FSC),linetype="dashed",size=1)+
  geom_vline(xintercept = 147.102,size=1,color="blue")+
  annotate("text", x=170, y=0.0045, label="10",size=5)+
  theme_bw()+
  theme(legend.position = "top")+ 
  ylim(0.000,0.005)+
  theme(legend.title=element_blank())+
  ggtitle("FSC")+
  theme(plot.title = element_text(size=22))


################
t.test(familyFinalData$Duration~fFamilyFinalData$Housing)
t.test(familyFinalData$Duration~familyFinalData$BasicNeeds)
t.test(familyFinalData$Duration~familyFinalData$FSC)


#non parametric version 
wilcox.test(familyFinalData$Duration~familyFinalData$Housing) 

###############
housingmean<-mutate(housingmean,Service="Housing")
colnames(housingmean)[1]<-"Status"
colnames(housingmean)[2]<-"Duration"
basicneedsmean<-mutate(basicneedsmean,Service="BasicNeeds")
colnames(basicneedsmean)[1]<-"Status"
colnames(basicneedsmean)[2]<-"Duration"
meanduration<-rbind(housingmean,basicneedsmean)
meanduration$Service<-factor(meanduration$Service, levels=c("Housing","BasicNeeds")) 
meanduration$Duration<-round(meanduration$Duration,2)

ggplot(meanduration,aes(x=Service,y=Duration,fill=Status))+
  geom_col(position="dodge",alpha=0.6,width = 0.5)+
  ylim(0,160)+
  geom_text(aes(label = Duration,vjust = -0.5, hjust = 0.5, color = "Status", size=3), show.legend  = FALSE)+
  theme(legend.title=element_blank())+
  ggtitle("Mean of duration")+
  theme(plot.title = element_text(size=22))

#needs to be made into two 

data2<-read.csv("typeCountsFinalData.csv")

ggplot(data2, aes(x=TypeCounts, y=CloseTimes,colour=PlacementAsY))+
  geom_jitter(shape=1)+
  geom_smooth(method=lm, se=TRUE) +
  labs(color='Placement') +
  theme_bw()

ggplot(data2, aes(x=TypeCounts, y=Duration,colour=PlacementAsY))+
  geom_jitter(shape=1)+
  geom_smooth(method=lm, se=TRUE) +
  labs(color='Placement') +
  theme_bw()




