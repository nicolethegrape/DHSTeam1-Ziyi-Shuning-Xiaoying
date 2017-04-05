# get place data from ACCEPT_REASON in entryCorhort dataset

library(dplyr)

getPlaceFromACCEPT_REASON <- function(mergedData){
  placeData <- mergedData %>% 
    select(CLIENT_ID, CASE_ID, ACCEPT_REASON) %>% 
    mutate(isplaced=grepl("placed",ACCEPT_REASON)) %>% 
    group_by(CASE_ID) %>% 
    summarise(isPlacedFromAcceptReason=sum(isplaced) %>% as.logical)
  return(placeData)
}