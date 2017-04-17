# convert client-level-before-after-data into family-level-after-data for placement

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

