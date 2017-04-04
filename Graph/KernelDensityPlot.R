generateKernelDensity <- function(finalData){
  ggplot(data=finalData, aes(x = averNumServiceFamily)) + 
    geom_density(kernel = "gaussian") + 
    facet_grid(.~placement)
}