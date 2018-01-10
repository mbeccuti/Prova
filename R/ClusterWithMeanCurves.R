#' Mean Cluster Curves
#'
#'@description
#' Selecting a model among FCM, Malthus, Gompertz and Logistic model, MClusterWithMeanCurve() generates the
#' mean cluster curves in a single plot and in a plot per each cluster the curves that belongs to that.
#' Furthemore it saves all the principal informations about that clusterization, for instance mean curve
#' values and the cluster of belonging per each curve.
#'
#'
#' @param out.funcit Object of class funcyOutList, obtained by the function funcit from the package
#'                   "funcy" or, selecting a value for k (number of clusters) and h (dimension of the space of
#'                   of the cluster mean), from the function cluster_choice.
#' @param databaseTr List containing the number of observations per each curve (called LenCurv),
#'                and a data frame constituted from the curves' ID, observed values and the respective times,
#'                that might be truncated at a specific time or not.
#'                It is generated automatically from the function DataImport() or DataTruncation() if we want consider
#'                a truncation time.
#' @param feature String feature name, stored in the target file, to plot curves according to.
#' @param k Number of clusters.
#' @param model String model name, it is possible to choose one among FCM, Malthus, Gompertz and Logistic models.
#' @return List containing the cluster mean curves plot for the model choosen, the k growth curves plots divided
#'        depending on the cluster and a list of informations about the model clustered. The information list stores
#'        the mean cluster.
#' @examples
#'
#'
#' @import ggplot2, cowplot
#' @export
ClusterWithMeanCurve<-function(out.funcit,databaseTr,feature,k,model)
{
  # source("R/fitfclust.R")
  # source("R/Residuals.R")
  # source("R/Clustering.R")
  # source("R/cluster.symbol.R")

  symbols<-cluster.symbol(k)
  Information<-list()
  time <- sort(unique(databaseTr$Dataset$Time))

  if(model=="FCM")
  {
    Cluster(out.funcit)->classes->Information$classes
    out.fit<-out.funcit@models$fitfclust@fit
    fitfclust.curvepredIrreg(out.fit)$meancurves->meancurves->Information$meancurves
  }
  else{
    clustering(databaseTr,k,model) ->classification
    classification$cluster ->classes-> Information$classes
    classification$center -> Information$center
    classification$meancurves->meancurves->Information$meancurves
  }
  classificate <- rep(classes,databaseTr$LenCurv)
  curves <- data.frame(Times=databaseTr$Dataset$Time,Vol=databaseTr$Dataset$Vol,ID=databaseTr$Dataset$ID,Cluster=classificate,Info=rep(databaseTr$LabCurv[[paste(feature)]],databaseTr$LenCurv))
  Information$ClustCurve <- data.frame(merge(curves[,1:4],databaseTr$LabCurv[,c("ID",feature)],by="ID"))

  plot_data<-data.frame(time=rep(time,k),means=c(meancurves[,1:k]),clusters=rep(c(1:k),each=length(time)))
  PlotMeanCurveFCM<-ggplot()+
                    geom_line(data=plot_data, aes(x=time,y=means,group=clusters) )+
                    labs(title=paste(model," cluster mean curves"), x="Days", y = "Volume")+
                    theme(plot.title = element_text(hjust = 0.5))
 plots<-NULL

    col<-as.factor(unique(curves$Info))
    plots<-list()
    for(i in 1:k)
    {
      plots[[paste(symbols[i],"Cluster")]]<-ggplot()+
        geom_line(data=plot_data[plot_data$clusters==i,], aes(x=time,y=means),size =1.3 )+
        labs(title=paste(model,"",symbols[i],"Cluster"), x="Days", y = "Volume")+
        geom_line(data = curves[curves$Cluster==i,],aes(x=Times,y=Vol,group=ID,color=factor(feature)))+
        scale_colour_manual(values = col,limits=col, name=paste(feature))+
        theme(plot.title = element_text(hjust = 0.5))
    }
     plots[["ALL"]]<-plot_grid(plotlist = plots)
     plots$ALL

 return(list(plotMeanCurve=PlotMeanCurveFCM,plotsCluster=plots,Information=Information))
}


