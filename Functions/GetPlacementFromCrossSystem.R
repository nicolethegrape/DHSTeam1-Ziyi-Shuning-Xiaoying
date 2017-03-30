getPlaceData2 <- function(mergedData){
  placeData2 <- mergedData %>%
    select(CLIENT_ID, CASE_ID, CYF_KPL_MIN_ACTIVE, CYF_KPL_MAX_ACTIVE, 
           CYF_PL_O_MIN_ACTIVE, CYF_PL_O_MAX_ACTIVE, JPO_KPL_MIN_ACTIVE, 
           JPO_KPL_MAX_ACTIVE, JPO_PL_O_MIN_ACTIVE, JPO_PL_O_MAX_ACTIVE) %>%
    mutate(isplaced=!is.na(CYF_KPL_MIN_ACTIVE) | !is.na(CYF_KPL_MAX_ACTIVE) 
           | !is.na(CYF_PL_O_MIN_ACTIVE) | !is.na(CYF_PL_O_MAX_ACTIVE)
           | !is.na(JPO_KPL_MIN_ACTIVE) | !is.na(JPO_KPL_MAX_ACTIVE)
           | !is.na(JPO_PL_O_MIN_ACTIVE) | !is.na(JPO_PL_O_MAX_ACTIVE)
           ) %>%
    group_by(CASE_ID) %>%
    summarise(isPlacedFromCrossSystem=sum(isplaced) %>% as.logical)
  return(placeData2)
}

### find inconsistency with GetPlacementFromACCEPT_REASON
# inconsistencyCaseId <- placeData %>%
#   filter(placeData$chplaced != placeData2$placedafter09)%>%
#   select(CASE_ID)
#   
# inconsistencyCaseId2 <- inconsistencyCaseId$CASE_ID
# inconsistencyData <- arrange(filter(mergedData, CASE_ID %in% inconsistencyCaseId2), CASE_ID)

