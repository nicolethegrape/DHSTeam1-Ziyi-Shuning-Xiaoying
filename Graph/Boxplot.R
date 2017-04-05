generateBoxPlot <- function(finalData, yVarName, title){
  ggplot(data=finalData, aes_string(x = "placement", y = yVarName)) + 
    geom_boxplot() +
    ggtitle(title)
}
