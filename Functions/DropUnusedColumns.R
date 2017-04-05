library(dplyr)
dropUnusedColumns <- function(entrycohortUnique){
  entrycohortUnique <- select(entrycohortUnique, c(CLIENT_ID:CASE_ID, )
}