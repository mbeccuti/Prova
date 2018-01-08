#
#'
#'
#' @param
#' @param
#' @return
#' @examples
#'
#'
#' @import
#' @export
source("R/multi_plot_function.R")

ClusterWithMeanCurve_plot<-function(out.funcit,databaseTr,Info,k,All=FALSE)
{

  symbols<- c("circle", "triangle","cross","times","diamond","reverse triangle","square cross","star","diamond", "plus","circle plus","triangles up and down","square plus","circle cross","square and triangle down","filled square","filled circle","filled triangle point-up","filled diamond","solid circle","bullet")

  classes<-Cluster(out.funcit)
  out.fit<-out.funcit@models$fitfclust@fit
  meancurves<-fitfclust.curvepredIrreg(out.fit)$meancurves
  classificate <- rep(classes,databaseTr$LenCurv.tr)
  curves <- data.frame(Cluster=classificate,Times=databaseTr$data.matrixtr$Time,Vol=databaseTr$data.matrixtr$Vol,ID=databaseTr$data.matrixtr$ID,Info=rep(databaseTr$LabCurv$Progeny,databaseTr$LenCurv.tr))
  time <- sort(unique(databaseTr$data.matrixtr$Time))


  plot_data<-data.frame(time=rep(time,k),means=c(meancurves[,1:k]),clusters=rep(c(1:k),each=length(time)))
  PlotMeanCurveFCM<-ggplot()+
                    geom_line(data=plot_data, aes(x=time,y=means,group=clusters) )+
                    labs(title="FCM cluster mean curves", x="Days", y = "Volume")+
                    theme(plot.title = element_text(hjust = 0.5))
 plots<-NULL
  if(All)
  {
    plots<-list()
    for(i in 1:k)
    {
      plots[[paste(symbols[i],"Cluster")]]<-ggplot()+
        geom_line(data=plot_data[plot_data$clusters==i,], aes(x=time,y=means) )+
        labs(title=paste(symbols[i],"Cluster"), x="Days", y = "Volume")+
        geom_line(data = curves[curves$Cluster==i,],aes(x=Times,y=Vol,group=ID),color=factor(curves$Info[curves$Cluster==i]))+
        theme(plot.title = element_text(hjust = 0.5))
    }
    return(list(plotMeanCurve=plotMeanCurveFCM,plotsCluster=plots))
  }

}


