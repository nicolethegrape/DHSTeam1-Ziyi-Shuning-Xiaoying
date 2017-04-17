library(dplyr)
library(ggplot2)

plotData <- read.csv("Data/plotData.csv")
percent2<-plotData$percent*100
plotData<-cbind(plotData,percent2)

plotData$serviceName <- as.factor(plotData$serviceName)
positions <- c("Housing", "Basic Needs", "FSC")

ggplot(plotData,aes(y=percent2, ymax=max(percent2)*1.50, x=serviceName, fill=serviceStatus, labels = percent2)) +
  geom_bar(stat = "identity",position = "dodge",alpha = 0.6, width = 0.5) +
  geom_text(aes(label = percent2), position = position_dodge(width=0.5), hjust=0.4, vjust=-0.5, fontface = "bold") + 
  ylab("Percentage of Children Placed") +
  xlab("") +
  ggtitle("Services and Placement") +
  scale_fill_discrete(name="Service Status") +
  scale_x_discrete(limits = positions) +
  theme_bw() +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 22, face = "bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=14,face="bold"),
        legend.position = "top")
  
