library(dplyr)
percent2<-plotData$percent*100
plotData<-cbind(plotData,percent2)

library("ggplot2")

plotData$serviceName <- as.factor(plotData$serviceName)


g<-ggplot(plotData,aes(y=percent2, x=serviceName, fill=serviceStatus))

g+geom_bar(stat = "identity",position = "dodge",alpha=0.6, width = 0.5) +
  geom_text(aes(label=percent2), position=position_dodge(width=0.5),hjust=0.4, vjust=-0.5, fontface = "bold") + 
  scale_fill_discrete(name="Service Status") +
  ylab("Percentage of Children Placed") +
  xlab("")+
  ggtitle("Services and Placement") +
  theme_bw() +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 22, face = "bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=14,face="bold"),
        legend.position = "top")
  
