library(ggplot2)

plotData <- read.csv("Data/plotData.csv")

ggplot(plotData, aes(serviceName, percent, fill = serviceStatus)) + 
  geom_bar(stat = "identity", position = "dodge", alpha=0.6)

ggplot(plotData, aes(serviceName, percent * 100, fill = serviceStatus)) + 
  geom_bar(stat = "identity", position = 'dodge') +
  geom_text(aes(label = percent * 100), position=position_dodge(width=0.5), vjust=-0.25)


ggplot(plotData,aes(y=percent * 100, x=serviceName, fill=serviceStatus)) +
  geom_bar(stat = "identity",position = "dodge",alpha=0.6, width = 0.5) + 
  geom_text(aes(label=percent * 100, position=position_dodge(width = 0.9), vjust=-0.25))+ 
  scale_fill_discrete(name="Service Status", labels=c("Post-CYF Service & NA","Pre-CYF Service")) +
  ylab("Percentage of Children Placed") +
  xlab("Services")


library(RcolorBrewer)
display.brewer.all()

