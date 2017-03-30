# housing: ACHA, HACP, HH
# behavior: DA
# nutrition: DPW_FS
# mental health: MH, ID

calNumService <- function(x){
  rows <- dim(x)[1]
  cols <- dim(x)[2]
  num <- 0
  for (j in seq(1, cols - 1, by = 2)){
    notAllNA <- FALSE
    for (i in 1:rows){
      if (!is.na(x[i, j]) | !is.na(x[i, j + 1])){
        notAllNA <- TRUE
        break
      }
    }
    if (notAllNA == TRUE){
      num <- num + 1
    }
  }
  return(num)
}

calKindService <- function(x){
  rows <- dim(x)[1]
  cols <- dim(x)[2]
  kind <- 0
  for (i in 1:rows){
    if (!is.na(x[i, 1]) | !is.na(x[i, 2]) | !is.na(x[i, 3]) | !is.na(x[i, 4]) | !is.na(x[i, 5]) | !is.na(x[i, 6])){
      kind <- kind + 1
      break
    }
  }
  for (i in 1:rows){
    if (!is.na(x[i, 7]) | !is.na(x[i, 8])){
      kind <- kind + 1
      break
    }
  }
  for (i in 1:rows){
    if (!is.na(x[i, 9]) | !is.na(x[i, 10])){
      kind <- kind + 1
      break
    }
  }
  for (i in 1:rows){
    if (!is.na(x[i, 11]) | !is.na(x[i, 12]) | !is.na(x[i, 13]) | !is.na(x[i, 14])){
      kind <- kind + 1
      break
    }
  }
  return(kind)
}

# calculate average num of services for each family
calAverNumServiceFamily <- function(mergedData){
  serviceData <- select(mergedData, CLIENT_ID, CASE_ID, ACHA_MIN_ACTIVE, ACHA_MAX_ACTIVE, 
           HACP_MIN_ACTIVE, HACP_MAX_ACTIVE, HH_MIN_ACTIVE, HH_MAX_ACTIVE, 
           DA_MIN_ACTIVE, DA_MAX_ACTIVE, DPW_FS_MIN_ACTIVE, DPW_FS_MAX_ACTIVE, 
           MH_MIN_ACTIVE, MH_MAX_ACTIVE, ID_MIN_ACTIVE, ID_MAX_ACTIVE) 
  KindServiceFamily <- as.numeric(by(serviceData[, 3:16], serviceData$CASE_ID, calKindService))
  NumHousingFamily <- as.numeric(by(serviceData[,3:8], serviceData$CASE_ID, calNumService))
  NumBehaviorFamily <- as.numeric(by(serviceData[,9:10], serviceData$CASE_ID, calNumService))
  NumNutritionFamily <- as.numeric(by(serviceData[,11:12], serviceData$CASE_ID, calNumService))
  NumMentalFamily <- as.numeric(by(serviceData[,13:16], serviceData$CASE_ID, calNumService))
  
  serviceDataNew <- data.frame(CASE_ID = sort(unique(serviceData$CASE_ID)), 
                               KindServiceFamily = KindServiceFamily,
                               NumHousingFamily = NumHousingFamily,
                               NumBehaviorFamily = NumBehaviorFamily,
                               NumNutritionFamily = NumNutritionFamily,
                               NumMentalFamily = NumMentalFamily)
  return(serviceDataNew)
}
