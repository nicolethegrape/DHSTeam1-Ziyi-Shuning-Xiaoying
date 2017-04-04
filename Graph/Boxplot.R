generateBoxPlot <- function(finalData, yDataName, title){
  ggplot(data=finalData, aes_string(x = "placement", y = yDataName)) + 
    geom_boxplot() +
    ggtitle(title)
}

generateKernelDensity <- function(finalData){
  ggplot(data=finalData, aes(x = averNumServiceFamily)) + 
    geom_density(kernel = "gaussian") + 
    facet_grid(.~placement)
}
