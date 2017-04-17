library(ggplot2)

conditional<-ggplot(ConditionalData,aes(y=percent,ymax=max(percent)*1.10, x=groups, fill=FSC))

# changeLabels <- function(x){
#   newLabels <- NULL
#   for (i in 1:length(x)){
#     if (x[i] == FALSE){
#       newLabels <- c(newLabels, "No FSC")
#     } else {
#       newLabels <- c(newLabels, "FSC")
#     }
#   }
#   return(newLabels)
# }

conditional+geom_bar(stat = "identity",position = "dodge",alpha=0.7, width = 0.5)  +
  geom_text(aes(label=percent), position=position_dodge(width=0.6), hjust=0.5, vjust=-1, fontface="bold") + 
  scale_x_discrete(
    labels=function(x) {
      sub(",",",\n",x,fixed=TRUE)
    }) +
  ylab("% of Placement") +
  xlab("")+ 
  ggtitle("Conditional Services and Placement") +
  scale_fill_discrete(labels=c("No FSC", "FSC")) +
  theme_bw() +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 22, face = "bold",hjust = 0.5),
        legend.text=element_text(size=12,face="bold"),
        legend.position = "top",
        legend.title=element_blank())

