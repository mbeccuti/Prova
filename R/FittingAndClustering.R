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
#' @import Residuals, Clustering, funcy
#' @export
FittingAndClustering<-function(Connector,h,k,FCM_all)
{
  dati<-Connector
  models <- c("Gompertz","Logistic","Malthus","FCM")
  syllab<-c("G","L","M","F")

  time <- dati$time.tr
  time_sorted <- sort(unique(dati$time.tr))
  x <- dati$x.tr
  grid <- dati$grid.tr
  curve <- dati$curve.tr
  colors <- dati$generations

  Connector_clustered<-vector("list",length(models))
  for(i in c(1:length(models)))
  {
    if(models[i]=="FCM")
    {
      out.funcit <- FCM_all[[paste("k=",k)]][[paste("h=",h)]]
      Cluster(out.funcit) -> Connector_clustered[[paste(models[i])]]$classes
      out.funcit@models$fitfclust@fit -> out.fit
      fitfclust.curvepredIrreg(out.fit)$meancurves -> Connector_clustered[[paste(models[i])]]$meancurves
    }
    else{
      Clustering(Connector,number_curves,k,models[i]) ->classification
      classification[["cluster"]] -> Connector_clustered[[paste(models[i])]]$classes
      classification[["center"]] -> Connector_clustered[[paste(models[i])]$center
      classification[["meancurves"]]->Connector_clustered[[paste(models[i])]]$meancurves
    }

    classificate <- rep(Connector_clustered[[paste(models[i])]]$classes,dati$lencurves)
    #colori <- rep(dati$generazione,dati$lunghezze)
    curves <- matrix(c(classificate,dati$time,dati$x,dati$curve),ncol=4)

    ### da cambiare in plotly!!!!!!!!!
    par(mfrow=c(round(k/2),as.integer((k/2))))
    for(i in c(1:k))
    {
      if(save=="TRUE")
      {
        pdf(file=paste("",models[i],"MeanGroup.pdf"),paper="a4r",width=11)
      }
      curves.i <- curves[curves[,1]==i,]
      index <- unique(curves.i[,4]) # curve nella classe i
      ni <- length(index)
      colori.i <- colors[index]
      temp <- sort(unique(curves[,2][curves[,1]==i]))
      temp <- temp[temp <= tr.day]
      meancurve <- meancurves[,i][time <= max(temp)]
      t <- time[time <= max(temp)]
      plot(t,meancurve,xlab='Days', ylab='Volume',type="l",col="black", xlim=c(min(grid),max(grid)+40),ylim=c(min(dati$x.tr),1600),cex=3,lwd=2.5,main=paste(syllab,i))
    }
  }
}
