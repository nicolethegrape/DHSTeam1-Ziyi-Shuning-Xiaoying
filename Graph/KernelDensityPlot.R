#generate kernel density graph 
generateKernelDensity <- function(finalData, xVarName,filter){
  p<-ggplot(data=finalData, aes_string(x = xVarName,fill=filter)) 
  p+geom_density(alpha=0.3)+
    ggtitle("Concurreny Serives and Children Placement Comparison Density Graph")+
    theme_bw()
}