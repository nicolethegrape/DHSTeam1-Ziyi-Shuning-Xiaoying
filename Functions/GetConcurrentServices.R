# change all date values into meaningful date values
mergedData
# CLIENT_ID CASE_ID


# determine concurrent (ACHA, DA...) services with CYF service for each individual
# ? the definition of concurrent
# the result should be:
# CLIENT_ID   CASE_ID   C_ACHA
# 688929      25532     0
# 688930      25532     -1
# 688931      25532     1

calConcurrentForClient <- function(newMergedData, concurrencyDataFrame){
  rows <- dim(newMergedData)[1]
  cols <- dim(newMergedData)[2]
  
  idxMin <- regexpr("MIN", names(mergedData)[seq(5, length(mergedData) - 1, by = 2)]) 
  idxMin <- idxMin - 2
  serviceNames <- substr(names(mergedData)[seq(5, length(mergedData) - 1, by = 2)], 1, idxMin)
  l <- length(serviceNames)
  concurrencyDataFrame <- data.frame(matrix(ncol = l + 2, nrow = 0))
  colnames(concurrencyDataFrame) <- c("CLIENT_ID", "CASE_ID", serviceNames)
  
  for (i in 1:rows){
    client_id = newMergedData[i, 1]
    case_id = newMergedData[i, 2]
    S <-  as.POSIXct('2002-06-30 20:00:00') # newMergedData[i, 3]
    E <-  as.POSIXct('2002-06-30 20:00:00') # newMergedData[i, 4]
    concurrencyVector <- c(client_id, case_id)
    for (j in 5:cols - 1){
      MIN <-  as.POSIXct('2002-05-30 20:00:00') # newMergedData[i, j]
      MAX <-  as.POSIXct('2002-05-30 20:00:00') # newMergedData[i, j + 1]
      concurrency <- 0
      if (MAX < S){
        concurrency <- -1
      } else if (MIN < E){
        concurrency <- 1
      }
      concurrencyVector <- c(concurrencyVector, concurrency)
    }
    concurrencyDataFrame[nrow(concurrencyDataFrame)+1,] <- concurrencyVector
  }
  return(concurrencyDataFrame)
}


# determine concurrent services with CYF service for each family
# ? divided by the number of family members / divided by the total number of services

library(dplyr)
#function: calculate the ratio of concurrent services
calRatioofConcurrent<-function(service) {
  numofzero<-length(which(service==0))
  numofpeople<-length(service)
  ratio<-numofzero/numofpeople
  return(ratio)
}

#get the final dataframe
ConcurrencyRatioData<-function(ConcurrencyData) {
  ConcurrecyRatioData<-ConcurrencyData %>%
    group_by(Case_ID) %>%
    summarise_each(funs(calRatioofConcurrent),ACHA:ID)
}

#THIS IS JUST FOR TEST
###########
# CLIENT_ID<-c(1234:1241)
# Case_ID<-c(1,1,1,1,2,2,2,2)
# ACHA<-c(1,0,0,1,0,0,1,-1)
# Mental<-c(1,0,0,-1,0,0,0,1)
# ID<-c(1,0,0,-1,1,1,-1,0)
# tempdata<-data.frame(CLIENT_ID,Case_ID,ACHA,Mental,ID)
# tempdatanew<-ConcurrencyRatioData(tempdata)

#Ziyi's commment
#不知道dplry包能不能定位第几列（就像[,10]这种），现在只能根据names来

