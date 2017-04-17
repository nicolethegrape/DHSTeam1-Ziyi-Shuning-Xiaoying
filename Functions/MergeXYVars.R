
mergeXYVars <- function(xVars, placeAsY, durationAndCloseTimes){
  familyFinalData <- cbind.data.frame(xVars, placeAsY[,2], durationAndCloseTimes[,2:3])
  colnames(familyFinalData) <- c(colnames(xVars), "PlacementAsY", "CloseTimes", "Duration")
  return(familyFinalData)
}