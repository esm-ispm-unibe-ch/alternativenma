# Forest plot with effects of all treatments verus average
#
# This function draws a forest plot with effects of all treatments versus a treatment of average performance
# in the active graphics window using an alternative parametrisation of the network meta-analysis model.
# altnmaobject: An object of class alternativenma
# eform: A logical indicating whether the exponential of effects should be drawn 
# ordered: A logical indicating whether the effects should be drawn ordered  

plot.alternativenma<-function(altnmaobject, eform=TRUE, ordered=TRUE){

  library(ggplot2)
  library(grid)
  
  #####################set values#############################

  if (ordered==TRUE){
    averages1=altnmaobject$averages[order(altnmaobject$averages$TE),]
  }
  
  if (ordered==FALSE){
    averages1=altnmaobject$averages
  }
  
  if (eform==TRUE){
    NetwTE=exp(averages1$TE)
    NetworkLCI=exp(averages1$lower)
    NetworkUCI=exp(averages1$upper)
  }
  if (eform==FALSE){
    NetwTE=averages1$TE
    NetworkLCI=averages1$lower
    NetworkUCI=averages1$upper
  }

  steps=length(NetwTE):1
  Effects=data.frame(NetwTE,steps)
  
  NetwCI=c(NetworkLCI,NetworkUCI)
  NetwEffect=c(NetwTE,NetwTE)
  StepsForPlot=c(steps,steps)
  ForReapPlot=data.frame(NetwCI,NetwEffect,StepsForPlot)
  
  ############################forest plot################################

    p=ggplot(Effects)+
      geom_point(aes(Effects$NetwTE,Effects$steps),na.rm = T)
    p=p+geom_line(data=ForReapPlot,aes(NetwCI,StepsForPlot,group=StepsForPlot),na.rm = T)
    p=p+geom_vline(xintercept = 1)

    p=p + theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
    
    p=p + annotate("text", x = rep(0.6, length(rownames(averages1))),
                 y = steps, label = c(rownames(averages1)))
    
    p=p + annotate("text", x = rep(1.6, length(rownames(averages1))),
                 y = steps, label = paste(round(NetwTE,2),"(",round(NetworkLCI,2),",",round(NetworkUCI,2),")"))
    
    p
}




