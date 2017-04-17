library(ggplot2)

conditional<-ggplot(ConditionalData,aes(y=percent, x=groups, fill=FSC))

conditional+geom_bar(stat = "identity",position = "dodge",alpha=0.6, width = 0.5)  +
  geom_text(aes(label=percent), position=position_dodge(width=0.6), hjust=0.5, vjust=-1) + 
  scale_fill_discrete(name="Family Support Center Service") +
  scale_x_discrete(
    labels=function(x) {
      sub(",",",\n",x,fixed=TRUE)
    }) +
  ylab("Percentage of Children Placed") +
  xlab("")+ 
  ggtitle("Services and Placement") +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=14,face="bold"))

