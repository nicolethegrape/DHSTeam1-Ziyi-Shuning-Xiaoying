rm(list = ls())
setwd("~/Desktop/2017Spring/R/DHSTeam1/TEAM1FinalCode")

# library all the packages
library(dplyr)
library(readr)
library(readxl)
library(zoo)
library(ggplot2)

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

##########################################
# get durationAndCloseTimes 
durationAndCloseTimes <- read.csv("DurationAndCloseTimes.csv")


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

# get the final complete data

xVars <- cbind.data.frame(familyPreCYFCatData, typeCountsData)
CompleteXYData <- mergeXYVars(xVars, familyPlacePostCYFData, durationAndCloseTimes)
write.csv(CompleteXYData, "CompleteXYData.csv", row.names=FALSE)


####################Part2########################
#####################ggPlot#####################



## Graph1##
##Duration Days Count##-Zhehan
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


## Graph 4 #！！！！！wrong version  update needed#
##Service Type Counts##----Duo
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

pie3D(x,labels=piepercent, explode=0.1, 
      main="Pie Chart of Service Type Count")
legend("top", legend=c("0", "1", "2", "3", "4", "5", "6", "7", "8"), 
       cex = 0.8, horiz = T, fill = rainbow(length(x)))


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




