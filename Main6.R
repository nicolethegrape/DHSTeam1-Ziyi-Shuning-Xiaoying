library(dplyr)
setwd("~/Desktop/Capstone/DHSTeam1-Ziyi-Shuning-Xiaoying")

finalDat <- read.csv("Data/FamilyFinalData.csv")

# x is housing, y is basic needs
genFourGroups <- function(x, y){
  groups <- NULL
  len <- length(x)
  for (i in 1:len){
    group <- 0
    if (x[i] == FALSE & y[i] == FALSE){
      group <- 1
    } else if (x[i] == TRUE & y[i] == FALSE){
      group <- 2
    } else if(x[i] == FALSE & y[i] == TRUE){
      group <- 3
    } else {
      group <- 4
    }
    groups <- c(groups, group)
  }
  return(groups)
}

groups <- genFourGroups(finalDat$Housing, finalDat$BasicNeeds)

test <- cbind.data.frame(finalDat, groups = groups)

dat <- test %>%
  group_by(groups, FSC) %>%
  
  