# Forest plot with effects of all treatments verus average
#
# This function draws a forest plot with effects of all treatments versus a treatment of average performance
# in the active graphics window using an alternative parametrisation of the network meta-analysis model.
# altnmaobject: An object of class alternativenma
# eform: A logical indicating whether the exponential of effects should be drawn 
# ordered: A logical indicating whether the effects should be drawn ordered  
# xmin: minimum limit of x axis
# xmax: maximum limit of x axis
# treatmentplacement: The x value of treatment labels
# effectplacement: The x value of the effect sizes


plot.alternativenma<-function(altnmaobject, eform=TRUE, ordered=TRUE, xmin=0.2, xmax=2.8, treatmentplacement=0.3, effectplacement=2.0){

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
    if (eform==TRUE){
      p=p+geom_vline(xintercept = 1)
    }else{
      p=p+geom_vline(xintercept = 0)
    }
    p=p + theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
    p=p + labs(x = "Effect vs fictional treatment of average performance")
    p=p + annotate("text", x = rep(treatmentplacement, length(rownames(averages1))),
                 y = steps, label = c(rownames(averages1)))
    
    p=p + annotate("text", x = rep(effectplacement, length(rownames(averages1))),
                 y = steps, label = paste(round(NetwTE,2),"(",round(NetworkLCI,2),",",round(NetworkUCI,2),")"))
    
    p + expand_limits(x=c(xmin,xmax))
}




