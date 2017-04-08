# Convert date in mergedDate into Date type.

convertDate<-function(mergedData){
  library(zoo)
  DateDat<-mergedData
  for(i in c(3:45)){
    DateDat[,i]<-as.Date(as.yearmon(DateDat[,i],"%b-%Y"))
  }
  return(DateDat)
}
