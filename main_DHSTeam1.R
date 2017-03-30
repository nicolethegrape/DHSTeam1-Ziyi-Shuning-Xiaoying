### Get Placement From ACCEPT_REASON in EntryCohort Dataset

library(dplyr)
# from original data to mergedData
lengthunique<- function(x) {
  length(unique(x))
} 
mergeData <- function(entrycohort, crosssystem){
  listOfClient <- sort(unique(entrycohort$CLIENT_ID))
  ncase <- tapply(entrycohort$CASE_ID, entrycohort$CLIENT_ID, lengthunique)
  multiCaseId <- which(ncase > 1)
  multiCaseClient <- listOfClient[multiCaseId]
  multiCaseIdCohort <- which(entrycohort$CLIENT_ID %in% multiCaseClient) # matches any of multiCaseClient
  entrycohort <- entrycohort[-multiCaseIdCohort,]
  entrycohortUnique <- entrycohort[!duplicated(entrycohort$CLIENT_ID),] 
  entrycohortUnique <- entrycohortUnique[,-c(14, 15)]
  mergedData <- merge(x = entrycohortUnique, y = crosssystem, by = "CLIENT_ID") # inner join
  return(mergedData)
}
# get placement data
getPlaceData <- function(mergedData){
  placeData <- mergedData %>% 
    select(CLIENT_ID, CASE_ID, ACCEPT_REASON) %>% 
    mutate(isplaced=grepl("placed",ACCEPT_REASON)) %>% 
    group_by(CASE_ID) %>% 
    summarise(isPlacedFromAcceptReason=sum(isplaced) %>% as.logical)
  return(placeData)
}

### Get Placement From Cross System Dataset

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

# housing: ACHA, HACP, HH
# behavior: DA
# nutrition: DPW_FS
# mental health: MH, ID

### Get Number Of Services 1
### Number of Services Per Person For Each Family
### sum / n()

# calculate num of services for each person
calNumServicePerson <- function(x){
  l <- length(x)
  num <- 0
  for (i in seq(1, l - 1, by = 2)){
    if (!is.na(x[i]) | !is.na(x[i + 1])){
      num <- num + 1
    }
  }
  return(num)
}

# calculate average num of services for each family
calAverNumServiceFamily <- function(mergedData){
  serviceData <- mergedData %>%
    select(CLIENT_ID, CASE_ID, ACHA_MIN_ACTIVE, ACHA_MAX_ACTIVE, 
           HACP_MIN_ACTIVE, HACP_MAX_ACTIVE, HH_MIN_ACTIVE, HH_MAX_ACTIVE, 
           DA_MIN_ACTIVE, DA_MAX_ACTIVE, DPW_FS_MIN_ACTIVE, DPW_FS_MAX_ACTIVE, 
           MH_MIN_ACTIVE, MH_MAX_ACTIVE, ID_MIN_ACTIVE, ID_MAX_ACTIVE) 
  numServicePerson <- apply(serviceData[,-(1:2)], 1, calNumServicePerson)
  numHousingServicePerson <- apply(serviceData[,3:8], 1, calNumServicePerson)
  numBehaviorServicePerson <- apply(serviceData[,9:10], 1, calNumServicePerson)
  numNutritionServicePerson <- apply(serviceData[,11:12], 1, calNumServicePerson)
  numMentalServicePerson <- apply(serviceData[,13:16], 1, calNumServicePerson)
  serviceData <- mutate(serviceData, numServicePerson = numServicePerson,
                        numHousingServicePerson = numHousingServicePerson,
                        numBehaviorServicePerson = numBehaviorServicePerson,
                        numNutritionServicePerson = numNutritionServicePerson,
                        numMentalServicePerson = numMentalServicePerson)
  serviceDataNew <- serviceData %>%
    group_by(CASE_ID) %>%
    summarise(averNumServiceFamily = sum(numServicePerson)/n(),
              averNumHousingFamily = sum(numHousingServicePerson)/n(),
              averNumBehaviorFamily = sum(numBehaviorServicePerson)/n(),
              averNumNutritionFamily = sum(numNutritionServicePerson)/n(),
              averNumMentalFamily = sum(numMentalServicePerson)/n())
  return(serviceDataNew)
}

### Get Number Of Services 2
### Kind of Services (overall), Number of Services for Each Family

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

calNumServiceFamily <- function(mergedData){
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

### Merge Service Data and Placement Data
# get final data
getFinalData <- function(placeData, serviceData){
  print(paste("Does the case ids in place data and service data match?", 
              all(placeData$CASE_ID == serviceData$CASE_ID)))
  finalData <- serviceData %>%
    mutate(placement = placeData$isPlacedFromAny)
  return(finalData)
}

### Boxplot

generateBoxPlot <- function(finalData, yDataName, title){
  ggplot(data=finalData, aes_string(x = "placement", y = yDataName)) + 
    geom_boxplot() +
    ggtitle(title)
}

### Multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

### TFPlot
# boxplot with facet_grid
generateTFPlot<-function(a,b,c,d,e,f){
  numService<-c(a,b,c,d,e)
  nameService<-c(rep("Total",1549),rep("Housing",1549),rep("Behavior",1549),rep("Nutrition",1549),rep("Mental",1549))
  placed<-rep(f,5)
  datPlot<-data.frame(numService,services=as.factor(nameService),placed)
  p<-ggplot(datPlot, aes(services, numService)) 
  p+ geom_boxplot(varwidth = TRUE, outlier.colour = "grey")+
    facet_grid(.~placed)+
    ylab("numServiceFamily")+
    ggtitle("Number of Services and Children Placement Comparison Graph")
}


### Main when calculating number of services per person for each family

setwd("~/Desktop/Capstone") # change to where you put the entrycohort and crosssystem data

library(readxl)
dat1 <- read_excel("DHS_Case_Clients_2016EntryCohort.xlsx", 
                   sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
dat2 <- read_excel("DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs

mergedData <- mergeData(dat1, dat2)
placeData <- getPlaceData(mergedData)

placeData2 <- getPlaceData2(mergedData)
all(placeData$CASE_ID == placeData2$CASE_ID) # TRUE
table(placeData$isPlacedFromAcceptReason) # from ACCEPT_REASON
table(placeData2$isPlacedFromCrossSystem) # from CrossSystem

# make placement TRUE if either of them TRUE
placeDataNew <- placeData %>%
  mutate(isPlacedFromCrossSystem = placeData2$isPlacedFromCrossSystem) %>%
  mutate(isPlacedFromAny= isPlacedFromAcceptReason | isPlacedFromCrossSystem)
table(placeDataNew$isPlacedFromAny)

serviceDataNew <- calAverNumServiceFamily(mergedData)

finalData <- getFinalData(placeDataNew, serviceDataNew)

library(ggplot2)
generateBoxPlot(finalData, "averNumServiceFamily", "Number of Services and Child Placement")
g1 <- generateBoxPlot(finalData, "averNumHousingFamily", "Number of Housing Service Per Person and Child Placement")
g2 <- generateBoxPlot(finalData, "averNumBehaviorFamily", "Number of Behavior Service Per Person and Child Placement")
g3 <- generateBoxPlot(finalData, "averNumNutritionFamily", "Number of Nutrition Service Per Person and Child Placement")
g4 <- generateBoxPlot(finalData, "averNumMentalFamily", "Number of Mental Service Per Person and Child Placement")

multiplot(g1, g2, g3, g4, cols=2)

generateTFPlot(finalData$averNumServiceFamily,finalData$averNumHousingFamily,finalData$averNumBehaviorFamily,finalData$averNumNutritionFamily,finalData$averNumMentalFamily,finalData$placement)

# since 0.5 service per person does not make sense,
# this is a second way of calculating number of services
### Main when calculating kind of services and number of services for each family

serviceDataNew2 <- calNumServiceFamily(mergedData)

finalData <- getFinalData(placeDataNew, serviceDataNew2)

library(ggplot2)
generateBoxPlot(finalData, "KindServiceFamily", "Number of Service Kinds and Child Placement")
g1 <- generateBoxPlot(finalData, "NumHousingFamily", "Number of Housing Service and Child Placement")
g2 <- generateBoxPlot(finalData, "NumBehaviorFamily", "Number of Behavior Service and Child Placement")
g3 <- generateBoxPlot(finalData, "NumNutritionFamily", "Number of Nutrition Service and Child Placement")
g4 <- generateBoxPlot(finalData, "NumMentalFamily", "Number of Mental Service and Child Placement")

multiplot(g1, g2, g3, g4, cols=2)

generateTFPlot(finalData$KindServiceFamily,finalData$NumHousingFamily,finalData$NumBehaviorFamily,finalData$NumNutritionFamily,finalData$NumMentalFamily,finalData$placement)


