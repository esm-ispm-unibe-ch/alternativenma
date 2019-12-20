# Plot with z-scores of all treatments verus average
#
# This function draws a line with z-scores of all treatments versus a treatment of average performance
# in the active graphics window using an alternative parametrisation of the network meta-analysis model.
# altnmaobject: An object of class alternativenma
# toplot: A character indicating whether "Zscores" or "Pscoreaverage" are to be drawn. 

classifier<-function(altnmaobject,toplot="Pscoreaverage"){
  
  library(ggplot2)
  library(grid)
  library(ggrepel)
  
  low.limit=ifelse(toplot=="Pscoreaverage",0,
                   -round(max(abs(min(altnmaobject$averages$Zscores)),max(altnmaobject$averages$Zscores)),1))
  upper.limit=ifelse(toplot=="Pscoreaverage",1,
                     round(max(abs(min(altnmaobject$averages$Zscores)),max(altnmaobject$averages$Zscores)),1))
  
  plotobject=altnmaobject$averages[,toplot]
 
  y = seq(low.limit, upper.limit,by=0.001)
  vals <- data.frame(x = rep(1,length(y)),y=y) #Range for x-values
  
  Effect=data.frame(altnmaobject$averages,plotobject)
  
  mytheme=theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.line.y = element_line(colour = "grey22", size = 1, linetype = "solid"),
                panel.grid.major  = element_line(color = "grey90"),
                panel.grid.minor  = element_line(color = "grey90"),
                panel.background = element_rect(fill = "grey90"),
                plot.margin = margin(1,1,1,1, "cm"),
                plot.background = element_rect(
                  fill = "grey90",
                  colour = "white",
                  size = 1)
                )
  
  p=ggplot(vals) +
    geom_line(aes(x,y,colour = y),size=8, show.legend=FALSE) +
    scale_colour_gradient2(low='coral3',mid='gray80',high='seagreen',midpoint=(upper.limit+low.limit)/2)+
    xlim(0.5,2.5)+
    expand_limits(y=c(low.limit, upper.limit)) + 
    geom_point(data=Effect,aes(x=1,plotobject),size=2,colour="black")+
    coord_fixed(ratio=(4-max(nchar(rownames(Effect)))*0.1)/((upper.limit-low.limit)/2)) +
    scale_y_continuous(breaks=c(seq(low.limit,upper.limit,by=ifelse(toplot=="Pscoreaverage",0.1,1)),
                                low.limit,upper.limit,(upper.limit+low.limit)/2))+
    labs(y = "Ranking of treatment compared to fictional treatment of average performance")+ 
    geom_text_repel(data=Effect,aes(x=1,plotobject,label=rownames(Effect)),hjust=0.5,direction = "y",
                    nudge_x= 1,angle= 0,segment.size = 1)+
    mytheme
  
  p

}

