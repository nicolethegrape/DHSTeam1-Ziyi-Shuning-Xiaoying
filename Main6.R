library(dplyr)
setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying")

finalDat <- read.csv("Data/FamilyFinalData.csv")

# x is housing, y is basic needs
genFourGroups <- function(x, y){
  groups <- NULL
  len <- length(x)
  for (i in 1:len){
    group <- ""
    if (x[i] == FALSE & y[i] == FALSE){
      group <- "No Housing, No Basic Needs"
    } else if (x[i] == TRUE & y[i] == FALSE){
      group <- "Yes Housing, No Basic Needs"
    } else if(x[i] == FALSE & y[i] == TRUE){
      group <- "No Housing, Yes Basic Needs"
    } else {
      group <- "Yes Housing, Yes Basic Needs"
    }
    groups <- c(groups, group)
  }
  return(groups)
}

groups <- genFourGroups(finalDat$Housing, finalDat$BasicNeeds)

test <- cbind.data.frame(finalDat, groups = groups)

dat <- test %>%
  group_by(groups, FSC) %>%
  summarise(percent = round(length(which(PlacementAsY == TRUE)) / n() * 100, 1))
 
write.csv(dat, "Data/ConditionalData.csv", row.names = FALSE)

