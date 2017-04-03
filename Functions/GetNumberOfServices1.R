# housing: ACHA, HACP, HH
# behavior: DA
# nutrition: DPW_FS
# mental health: MH, ID

# Definition 1:
# CASE_ID	CLIENT_ID	ACHA_MIN	ACHA_MAX	HACP_MIN	HACP_MAX
#  29003	720332	  AUG-2005	JAN-2011	NA	      NA
#  29003	720341	  NOV-2007	JAN-2011	NA	      NA
#  29003	759849	  NA	      NA	      AUG-2005	JAN-2011
#  29003	912237	  NA	      NA	      NOV-2007	JAN-2011
#	The average number of ACHA service received per person in one family:
# 2 ACHA services / 4 members total = 0.5 ACHA service per person for case 29003.

#	The average number of housing services (ACHA + HACP, just for here) received per person in one family:
# (2 ACHA services + 2 HACP services) / 4 members total = 1 housing service per person for case 29003.


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

# calculate the average num of services per person for each family
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
