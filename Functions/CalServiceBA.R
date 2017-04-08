#calculate before and after for merged data 
calServiceBA <- function(newMergedData){
  rows <- dim(newMergedData)[1]
  cols <- dim(newMergedData)[2]
  
  idxMax <- regexpr("MAX", names(newMergedData)[seq(5, length(newMergedData), by = 2)]) 
  idxMax <- idxMax - 2
  serviceNames <- substr(names(newMergedData)[seq(5, length(newMergedData), by = 2)], 1, idxMax)
  l <- length(serviceNames)
  concurrencyDataFrame <- data.frame(matrix(ncol = l + 2, nrow = 0))
  colnames(concurrencyDataFrame) <- c("CLIENT_ID", "CASE_ID", serviceNames)
  
  for (i in 1:rows){
    client_id = newMergedData[i, 1]
    case_id = newMergedData[i, 2]
    A <- as.Date(newMergedData[i, 3])
    E <- as.Date(newMergedData[i, 4])
    concurrencyVector <- c(client_id, case_id)
    for (j in seq(5,cols,by = 2)){
      MAX <- as.Date(newMergedData[i, j])
      concurrency <- NA
      
      if (is.na(MAX)) {
        concurrency<-0
      } else if (MAX>=A) {
        concurrency <- 1
      } else if (MAX<A) {
        concurrency<--1
      }
      concurrencyVector[length(concurrencyVector) + 1] <- concurrency
    }
    concurrencyDataFrame[nrow(concurrencyDataFrame)+1,] <- concurrencyVector
  }
  return(concurrencyDataFrame)
}
