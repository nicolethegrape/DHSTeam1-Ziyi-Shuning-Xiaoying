# drop unused data, only keep CLIENT_ID, CASE_ID, CL_INLV_START, LAST_CLOSE_DT, ACHA_MIN_ACTIVE:ID_MAX_ACTIVE   

library(dplyr)
keepOnlyService <- function(mergedData){
  mergedData <- mergedData %>% 
    select(c(CLIENT_ID:CASE_ID, SRVC_ACPT_DT_MY, ACHA_MIN_ACTIVE:ID_MAX_ACTIVE))   
}
