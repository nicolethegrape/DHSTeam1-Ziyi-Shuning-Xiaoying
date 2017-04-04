generateBoxPlot <- function(finalData, yDataName, title){
  ggplot(data=finalData, aes_string(x = "placement", y = yDataName)) + 
    geom_boxplot() +
    ggtitle(title)
}
