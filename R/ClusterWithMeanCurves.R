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
#' @param database List containing the number of observations per each curve (called LenCurv),
#'                and a data frame constituted from the curves' ID, observed values and the respective times,
#'                that might be truncated at a specific time or not.
#'                It is generated automatically from the function DataImport() or DataTruncation() if we want consider
#'                a truncation time.
#' @param k Number of clusters.
#' @param model String model name, it is possible to choose one among FCM, Malthus, Gompertz and Logistic models.
#' @param feature String feature name, stored in the target file, to plot curves according to.
#' @return List containing the cluster mean curves plot for the model choosen, the k growth curves plots divided
#'        depending on the cluster and a list of informations about the model clustered. The information list stores
#'        the mean cluster.
#' @examples
#'
#'
#' @import ggplot2 cowplot
#' @export
ClusterWithMeanCurve<-function(out.funcit,database,k,model,feature)
{

  symbols<-cluster.symbol(k)
  Information<-list()
  time <- sort(unique(database$Dataset$Time))

  if(model=="FCM")
  {
    Cluster(out.funcit)->classes->Information$classes
    out.fit<-out.funcit@models$fitfclust@fit

    # Check if it is regular
    if(out.funcit@reg==1)
    {
      fitfclust.curvepred(out.fit)$meancurves->meancurves->Information$meancurves
    } else{
      fitfclust.curvepredIrreg(out.fit)$meancurves->meancurves->Information$meancurves
    }
  }
  else{
    clustering(database,k,model) ->classification
    classification$cluster ->classes-> Information$classes
    classification$center -> Information$center
    classification$meancurves->meancurves->Information$meancurves
  }
  classificate <- rep(classes,database$LenCurv)
  curves <- data.frame(Times=database$Dataset$Time,Vol=database$Dataset$Vol,ID=database$Dataset$ID,Cluster=classificate, Info=rep(t(database$LabCurv[feature]),database$LenCurv))
  Information$ClustCurve <- data.frame(merge(curves[,1:4],database$LabCurv,by="ID"))

  # cut the meancurves at the growth curves' maximum time
  time1<-sort(unique(database$Dataset$Time))
  meancurves_truncated<-c()
  time3<-c()
  cluster<-c()
  for(clust in 1:k)
  {
    time2<-sort(unique(curves[curves$Cluster==clust,]$Times))
    m<-meancurves[,clust][time1<=max(time2)]
    time3<-c(time3,time1[time1<=max(time2)])
    meancurves_truncated<-c(meancurves_truncated,m)
    cluster<-c(cluster,rep(clust,length(time1[time1<=max(time2)])))
  }



  plot_data<-data.frame(time=time3,means=meancurves_truncated,clusters=cluster)
  PlotMeanCurveFCM<-ggplot()+
                    geom_line(data=plot_data, aes(x=time,y=means,group=clusters) )+
                    labs(title=paste(model," cluster mean curves"), x="Days", y = "Volume")+
                    theme(plot.title = element_text(hjust = 0.5))

    col<-as.character(unique(curves$Info))
    col1<-rainbow(length(col))
    plots<-list()
    for(i in 1:k)
    {
      plots[[paste(symbols[i],"Cluster")]]<-ggplot()+
        geom_line(data=plot_data[plot_data$clusters==i,], aes(x=time,y=means),size =1.3 )+
        labs(title=paste(model,"",symbols[i],"Cluster"), x="Days", y = "Volume")+
        geom_line(data = curves[curves$Cluster==i,],aes(x=Times,y=Vol,group=ID,color=factor(Info)))+
        scale_colour_manual(values = col1,limits=col,breaks=col,name=feature)+
        theme(plot.title = element_text(hjust = 0.5))
    }
     plots[["ALL"]]<-plot_grid(plotlist = plots)
     plots$ALL

 return(list(plotMeanCurve=PlotMeanCurveFCM,plotsCluster=plots,Information=Information))
}


