# determine concurrent services with CYF service for each family
# ? divided by the number of family members / divided by the total number of services

library(dplyr)
#function: calculate the ratio of concurrent services
calRatio<-function(service) {
  numofzero<-length(which(service==0))
  numofpeople<-length(service)
  ratio<-numofzero/numofpeople
  return(ratio)
}

#get the final dataframe
calConcurrencyRatio<-function(ConcurrencyData) {
  ConcurrecyRatioData <- ConcurrencyData %>%
    group_by(CASE_ID) %>%
    summarise_each(funs(calRatio),ACHA:ID)
}