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






