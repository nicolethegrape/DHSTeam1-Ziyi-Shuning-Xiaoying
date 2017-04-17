
library(dplyr)

aggrFamilyPreCYFCat <- function(familyPreCYFData){
  names <- c("CASE_ID", "Placement", "Housing", "BasicNeeds", "FSC")
  placementTrue <- Reduce("|", select(familyPreCYFData, CYF_KPL, CYF_PL_O))
  housingTrue <- Reduce("|", select(familyPreCYFData, ACHA, HH, HACP))
  basicNeedsTrue <-  Reduce("|", select(familyPreCYFData, DPW_FS, DPW_GA, DPW_SSI, DPW_TANF))
  familyPreCYFAggrData <- cbind.data.frame(familyPreCYFData$CASE_ID, placementTrue, housingTrue, basicNeedsTrue, familyPreCYFData$FSC)
  colnames(familyPreCYFAggrData) <- names
  return(familyPreCYFAggrData)
}
