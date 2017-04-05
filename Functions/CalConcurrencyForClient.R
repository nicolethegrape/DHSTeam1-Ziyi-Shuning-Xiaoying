# determine concurrent (ACHA, DA...) services with CYF service for each individual
# ? the definition of concurrent
# the result should be:
# CLIENT_ID   CASE_ID   C_ACHA
# 688929      25532     0
# 688930      25532     -1
# 688931      25532     1

calConcurrencyForClient <- function(newMergedData){
  rows <- dim(newMergedData)[1]
  cols <- dim(newMergedData)[2]
  
  idxMin <- regexpr("MIN", names(newMergedData)[seq(5, length(newMergedData) - 1, by = 2)]) 
  idxMin <- idxMin - 2
  serviceNames <- substr(names(newMergedData)[seq(5, length(newMergedData) - 1, by = 2)], 1, idxMin)
  l <- length(serviceNames)
  concurrencyDataFrame <- data.frame(matrix(ncol = l + 2, nrow = 0))
  colnames(concurrencyDataFrame) <- c("CLIENT_ID", "CASE_ID", serviceNames)
  
  for (i in 1:rows){
    client_id = newMergedData[i, 1]
    case_id = newMergedData[i, 2]
    S <- as.Date(newMergedData[i, 3])
    E <- as.Date(newMergedData[i, 4])
    concurrencyVector <- c(client_id, case_id)
    for (j in seq(5,cols - 1,by = 2)){
      MIN <- as.Date(newMergedData[i, j])
      MAX <- as.Date(newMergedData[i, j + 1])
      concurrency <- NA
      
      if (!is.na(MIN) & !is.na(MAX)){
        if (MAX < S){
          concurrency <- -1
        } else if (MIN > E){
          concurrency <- 1
        } else {
          concurrency <- 0
        }
      }
      
      concurrencyVector[length(concurrencyVector) + 1] <- concurrency
    }
    concurrencyDataFrame[nrow(concurrencyDataFrame)+1,] <- concurrencyVector
  }
  return(concurrencyDataFrame)
}