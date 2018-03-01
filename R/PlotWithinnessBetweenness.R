#' Withinness & Betweenness Visualization
#'
#' Plot withinness and betweenness clustering measures
#'
#' @param ClustCurve A data frame with 5 arguments : time, volume, ID, cluster membership and feature values for each curves.
#' @param MeanCurves A matrix with the meancurves on the columns according to different clusters.
#' @param Title Title to associate to the withinness and Betweenness plot.
#' @param save A logical value. If "save" equals TRUE the plot is saved in a pdf file (the default is FALSE).
#' @param path A character string for saving plot path folder .If "save" is TRUE and "path" is missing, the plot is saved in the current directory.
#' @examples
#' @import ggplot2 ggforce
#' @export
PlotWithinnessBetweenness <- function(ClustCurve,MeanCurves,Title=NULL,save=TRUE,path=NULL)
{

  K <- length(unique(ClustCurve[,4]))
  ClustSymbol<-cluster.symbol(K)
  nfeature <- length(unique(ClustCurve[,5]))[1]
  cluster.palette <- rainbow(K)
  #feature.palette <- rainbow(nfeature+3)
  feature.lev <- sort(unique(ClustCurve[,5]))

  ## calculate the max, min, mean and sd distance in the k-clusters;
  within.all <- Withinness(ClustCurve,MeanCurves,centroids=TRUE)
  ## calculate the nearest cluster
  between.all <- Betweenness(ClustCurve,MeanCurves)$Betweenness
  ## sort the cluster and save the distance among them
  shift<-sort(Betweenness(ClustCurve,MeanCurves)$CentroidDist[1,])
  ClustSymbol.sorted <- names(shift)
  i<-match(ClustSymbol.sorted, ClustSymbol)

  cluster.magnitudo <- apply(table(unique(ClustCurve[,c(4,1)])),1,sum)[i]
  circles <- data.frame(
  x0 = numeric(3*K)     ,
  y0 = numeric(3*K)     ,
  r = numeric(3*K)      ,
  distance = numeric(3*K),
  Cluster = numeric(3*K)
  )
  colnames(circles) <- c("x0","y0","r","distance","Cluster")

  n <- length(unique(ClustCurve[,1]))
  WithDist <- data.frame(
  x1 = numeric(n),
  y1 = numeric(n),
  Cluster = numeric(n),
  feature = numeric(n)
	)
  colnames(WithDist) <- c("x1","y1","Cluster","feature")
  counter <- 1
  index <- matrix(seq(1,3*K),nrow=K,byrow=TRUE)

  for(k in 1:K)
  {
  ### Data frames
    dataplot <- DataFrameWithinness.i(ClustCurve,MeanCurves,i[k],ClustSymbol,shift=shift[i[k]])
    circles[index[k,],] <- dataplot$circles
    WithDist[counter:(cumsum(cluster.magnitudo)[k]),] <- dataplot$WithDist
    counter <- cumsum(cluster.magnitudo)[k] + 1
  }

  circles$distance <- factor(circles$distance)
  circles$Cluster <- factor(circles$Cluster)

  WithDist$Cluster <- factor(WithDist$Cluster)
  WithDist$feature <- factor(WithDist$feature)

  plots <- ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r,linetype=distance,color=Cluster), data=circles,size=1)
  plots <- plots + scale_shape_manual(values=c(0:(K-1)))+
                    geom_point(data=WithDist,aes(x=x1,y=y1,shape=Cluster),size=4)+
                    scale_linetype_manual("",values = c( "2"= "dashed","1"="solid"),
                                           labels=c("Mean ","Mean+- sd"))

  if(is.null(Title)) Title<-"Cluster betweenness and withinness"

  plots <- plots  + geom_text(aes(x=x0, y=y0,label=Cluster), data=circles) +
                    scale_colour_manual(values = cluster.palette,name="Cluster Colors") +
                    labs(title=Title,x="distance",y="distance")+
                    theme(plot.title = element_text(hjust = 0.5),                                                                      axis.line = element_line(colour = "black"))


  if(save==TRUE)
  {
   if(is.null(path)) path <- getwd()
   ggsave(filename="Betweenness&Withinness.pdf",plot =plots,width=29, height = 20, units = "cm",scale = 1,path=path)
  }

  return(plots)
}

