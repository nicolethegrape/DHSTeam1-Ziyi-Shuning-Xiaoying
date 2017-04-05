# from original data to mergedData
library(dplyr)
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
  entrycohortUnique <- entrycohort[!duplicated(entrycohort$CLIENT_ID),] #扔掉同一个CLIENT_ID的多条记录
  entrycohortUnique <- entrycohortUnique[,-c(14, 15)] #扔掉CLOSE_DT和CLOSE_REASON
  entrycohortUnique <- arrange(entrycohortUnique, CLIENT_ID)
  entrycohortUnique <- data.frame(entrycohortUnique, LAST_CLOSE_DT = lastClose)
  entrycohortUnique <- entrycohortUnique[,c(-(3:7), -(9:13))]
  mergedData <- merge(x = entrycohortUnique, y = crosssystem, by = "CLIENT_ID") # inner join
  return(mergedData)
}