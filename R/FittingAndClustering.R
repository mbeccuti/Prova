#' Fitting and Clustering
#'
#'@description
#' Using the function ClusterWithMeanCurve() it is possible to save the mean cluster curves in a single plot
#' and in a plot per each cluster the curves that belongs to that. With FittingAndClustering() is possible to
#' do that for the FCM, Malthus, Gompertz and Logistic models in order to compare their clusterizations.
#' These plots could be saved in a pdf per each model, furthermore it saves in a single pdf (MeanCurves.pdf) the
#' mean cluster curves for all the models.
#'
#'
#' @param databaseTr  List containing the number of observations per each curve (called LenCurv),
#'                and a data frame constituted from the curves' ID, observed values and the respective times,
#'                that might be truncated at a specific time or not.
#'                It is generated automatically from the function DataImport() or DataTruncation() if we want consider
#'                a truncation time.
#' @param k Number of clusters, it could be a vector.
#' @param h Dimension of the cluster mean space.
#' @param FCM_all List of the all funcit's outputs for each k and h  obtained from the function "cluster_choice".
#' @param feature String feature name, stored in the target file, to plot curves according to.
#' @param save When TRUE (the default is FALSE), it is possible to save the plots of the growth curves divided
#'             depending on the belonging cluster in a pdf per each model.
#' @param path Path to save plot to (combined with filename).
#' @return  List containing per each model the mean curves plot and the clustered growth curves plots
#'          and a list of informations about the model clustered.
#' @examples
#'
#'
#' @import ggplot2
#' @export
FittingAndClustering<-function(databaseTr,h,k,FCM_all,feature,save=FALSE,path=NULL)
{
  out<-list()
  out.funcit <-FCM_all[[paste("k=",k)]][[paste("h=",h)]]
  models<-c("FCM","Malthus","Gompertz","Logistic")
  for(i in models)
  {
    out[[paste(i)]]<-ClusterWithMeanCurve(out.funcit,databaseTr,feature =feature,k,i)

  }

  mcurves<-list(out$FCM$plotMeanCurve,out$Malthus$plotMeanCurve,out$Gompertz$plotMeanCurve,out$Logistic$plotMeanCurve)
  out$MeanCurves<-plot_grid(plotlist = mcurves)
  if(save==TRUE)
  {
     ggsave(filename="MeanCurves.pdf",plot =out$MeanCurves,width=29, height = 20, units = "cm",scale = 1,path = path )
     ggsave(filename = "FCMCluster.pdf",plot=out$FCM$plotsCluster$ALL,width=29, height = 20, units = "cm",scale = 1,path = path)
     ggsave(filename = "MalthusCluster.pdf",plot=out$Malthus$plotsCluster$ALL,width=29, height = 20, units = "cm",scale = 1,path = path)
     ggsave(filename = "GompertzCluster.pdf",plot=out$Gompertz$plotsCluster$ALL,width=29, height = 20, units = "cm",scale = 1,path = path)
     ggsave(filename = "LogisticCluster.pdf",plot=out$Logistic$plotsCluster$ALL,width=29, height = 20, units = "cm",scale = 1,path = path)
  }
  return(out)
}
