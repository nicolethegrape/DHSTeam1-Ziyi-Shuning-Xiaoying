library(dplyr)
# from original data to mergedData
lengthunique<- function(x) {
  length(unique(x))
} 
mergeData <- function(entrycohort, crosssystem){
  listOfClient <- sort(unique(entrycohort$CLIENT_ID))
  ncase <- tapply(entrycohort$CASE_ID, entrycohort$CLIENT_ID, lengthunique)
  multiCaseId <- which(ncase > 1)
  multiCaseClient <- listOfClient[multiCaseId]
  multiCaseIdCohort <- which(entrycohort$CLIENT_ID %in% multiCaseClient) # matches any of multiCaseClient
  entrycohort <- entrycohort[-multiCaseIdCohort,]
  entrycohortUnique <- entrycohort[!duplicated(entrycohort$CLIENT_ID),] #扔掉同一个CLIENT_ID的多条记录
  entrycohortUnique <- entrycohortUnique[,-c(14, 15)] #扔掉CLOSE_DT和CLOSE_REASON
  mergedData <- merge(x = entrycohortUnique, y = crosssystem, by = "CLIENT_ID") # inner join
  return(mergedData)
}
# get placement data
getPlaceData <- function(mergedData){
  placeData <- mergedData %>% 
    select(CLIENT_ID, CASE_ID, ACCEPT_REASON) %>% 
    mutate(isplaced=grepl("placed",ACCEPT_REASON)) %>% 
    group_by(CASE_ID) %>% 
    summarise(isPlacedFromAcceptReason=sum(isplaced) %>% as.logical)
  return(placeData)
}