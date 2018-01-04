#  Progeny deve diventare un parametro, potremmo chiamarlo feature
#'
#'
#' @param
#' @param
#' @return ...ClustCurve è utilizzato nelle funzioni che calcolano withness and betweenness
#' @examples
#'
#'
#' @import
#' @export
library("ggplot2")
library("cowplot")
source("R/fitfclust.R")
source("R/Residuals.R")
source("R/Clustering.R")
source("R/cluster.symbol.R")

ClusterWithMeanCurve_plot<-function(out.funcit,databaseTr,feature,k,All=FALSE,model)
{

  symbols<-cluster.symbol(k)
  Informations<-list()
  time <- sort(unique(databaseTr$Dataset$Time))

  if(model=="FCM")
  {
    Cluster(out.funcit)->classes->Informations$classes
    out.fit<-out.funcit@models$fitfclust@fit
    fitfclust.curvepredIrreg(out.fit)$meancurves->meancurves->Informations$meancurves
  }
  else{
    clustering(databaseTr,k,model) ->classification
    classification$cluster ->classes-> Informations$classes
    classification$center -> Informations$center
    classification$meancurves->meancurves->Informations$meancurves
  }
  classificate <- rep(classes,databaseTr$LenCurv)
  curves <- data.frame(Times=databaseTr$Dataset$Time,Vol=databaseTr$Dataset$Vol,ID=databaseTr$Dataset$ID,Cluster=classificate,Info=rep(databaseTr$LabCurv[[paste(feature)]],databaseTr$LenCurv))


  plot_data<-data.frame(time=rep(time,k),means=c(meancurves[,1:k]),clusters=rep(c(1:k),each=length(time)))
  PlotMeanCurveFCM<-ggplot()+
                    geom_line(data=plot_data, aes(x=time,y=means,group=clusters) )+
                    labs(title=paste(model," cluster mean curves"), x="Days", y = "Volume")+
                    theme(plot.title = element_text(hjust = 0.5))
 plots<-NULL
  if(All)
  {
    col<-as.factor(unique(curves$Info))
    # ,color=factor(curves$Info[curves$Cluster==i])
    plots<-list()
    for(i in 1:k)
    {
      plots[[paste(symbols[i],"Cluster")]]<-ggplot()+
        geom_line(data=plot_data[plot_data$clusters==i,], aes(x=time,y=means),size =1.3 )+
        labs(title=paste(model,"",symbols[i],"Cluster"), x="Days", y = "Volume")+
        geom_line(data = curves[curves$Cluster==i,],aes(x=Times,y=Vol,group=ID,color=factor(Info)))+
        scale_colour_manual(values = col,limits=col, name=paste(Info))+
        theme(plot.title = element_text(hjust = 0.5))
    }
     plots[["ALL"]]<-plot_grid(plotlist = plots)
     plots$ALL
  }
 return(list(plotMeanCurve=PlotMeanCurveFCM,plotsCluster=plots,Informations=Informations,ClustCurve=curves[,1:4]))
}


