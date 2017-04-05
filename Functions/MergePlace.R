# make placement TRUE if either of two place data is TRUE

mergePlace <- function(placeData1, placeData2){
  placeData <- placeData1 %>%
    mutate(isPlacedFromCrossSystem = placeData2$isPlacedFromCrossSystem) %>%
    mutate(isPlacedFromAny= isPlacedFromAcceptReason | isPlacedFromCrossSystem)
}