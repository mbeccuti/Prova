#' To write.....
#'
#' @param Connector the list in which is stored all the informations, output from the function:"DataInfo"
#' @param k the value of the cluster that you want consider.
#' @param h the value of the space dimension h that you want consider.
#' @param FCM_all the list of the all values of the command funcit for each k and h, output from the function:  cluster_choice
#' @return  For each fittig the line plot divided in the different clusters, CONNECTOR list clustered
#' @examples
#'
#'
#' @import ClusterWithMeanCurve_plot
#' @export



FittingAndClustering<-function(databaseTr,h,k,FCM_all,Info,save=FALSE)
{
  out<-list()
  out.funcit <-FCM_all[[paste("k=",k)]][[paste("h=",h)]]
  models<-c("FCM","Malthus","Gompertz","Logistic")
  for(i in models)
  {
    out[[paste(i)]]<-ClusterWithMeanCurve_plot(out.funcit,databaseTr,Info =paste(Info), k,All = TRUE,i)

  }
  mcurves<-list(out$FCM$plotMeanCurve,out$Malthus$plotMeanCurve,out$Gompertz$plotMeanCurve,out$Logistic$plotMeanCurve)
  out$MeanCurves<-plot_grid(plotlist = mcurves)
  if(save==TRUE)
  {
    ggsave(filename="MeanCurves.pdf",plot =out$MeanCurves,width=29, height = 20, units = "cm",scale = 1 )
    ggsave(filename = "FCMCluster.pdf",plot=out$FCM$plotsCluster$ALL,width=29, height = 20, units = "cm",scale = 1)
    ggsave(filename = "MalthusCluster.pdf",plot=out$Malthus$plotsCluster$ALL,width=29, height = 20, units = "cm",scale = 1)
    ggsave(filename = "GompertzCluster.pdf",plot=out$Gompertz$plotsCluster$ALL,width=29, height = 20, units = "cm",scale = 1)
    ggsave(filename = "LogisticCluster.pdf",plot=out$Logistic$plotsCluster$ALL,width=29, height = 20, units = "cm",scale = 1)

    dev.off()
  }
  return(out)
}
