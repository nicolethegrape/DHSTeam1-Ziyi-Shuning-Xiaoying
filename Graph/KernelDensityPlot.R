generateKernelDensity <- function(finalData, xVarName){
  ggplot(data=finalData, aes_string(x = xVarName)) + 
    geom_density(kernel = "gaussian") + 
    facet_grid(.~placement)
}