generateKernelDensity <- function(finalData, xVarName){
  ggplot(data=finalData, aes_string(x = xVarName,fill=placement)) + 
    geom_density(kernel = "gaussian",alpha=0.3) 
}