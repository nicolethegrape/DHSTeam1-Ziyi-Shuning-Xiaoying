# calculate how many types of pre-CYF services one family receives.
# Housing: ACHA, HH, HACP
# Basic Needs: DPW........
# FSC
library(dplyr)
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