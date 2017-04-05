# Convert date in mergedDate into Date type.

convertDate<-function(mergedDate){
  require(zoo)
  DateDat<-mergedDate
  for(i in 1:18){
    dat[,i]<-as.Date(as.yearmon(dat[,i],"%b-%Y"))
  }
  return(DateDat)
}