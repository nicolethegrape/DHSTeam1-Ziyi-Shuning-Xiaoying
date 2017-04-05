# Convert date in mergedDate into Date type.

convertDate<-function(mergedData){
  require(zoo)
  DateDat<-mergedData
  for(i in c(3,5:46)){
    DateDat[,i]<-as.Date(as.yearmon(DateDat[,i],"%b-%Y"))
  }
  return(DateDat)
}
