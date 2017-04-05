# merge place and service data

mergePlaceAndService <- function(placeData, serviceData){
  print(paste("Does the case ids in place data and service data match?", 
              all(placeData$CASE_ID == serviceData$CASE_ID)))
  finalData <- serviceData %>%
    mutate(placement = placeData$isPlacedFromAny)
  return(finalData)
}