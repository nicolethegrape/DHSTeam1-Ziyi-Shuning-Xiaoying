# boxplot with facet_grid

generateTFPlot<-function(a,b,c,d,e,f){
  numService<-c(a,b,c,d,e)
  nameService<-c(rep("Total",1549),rep("Housing",1549),rep("Behavior",1549),rep("Nutrition",1549),rep("Mental",1549))
  placed<-rep(f,5)
  datPlot<-data.frame(numService,nameService,placed)
  p<-ggplot(datPlot, aes(as.factor(nameService), numService)) 
  p+ geom_boxplot(varwidth = TRUE, outlier.colour = "grey")+facet_grid(. ~ placed)

}



