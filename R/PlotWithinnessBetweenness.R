#' Withinness & Betweenness Visualization
#'
#' Plot withinness and betweenness clustering measures  
#'
#' @param ClustCurve A data frame with 5 arguments : time, volume, ID, cluster membership and feature values for each curves.
#' @param MeanCurves A matrix with the meancurves on the columns according to different clusters.
#' @param save A logical value. If "save" equals TRUE the plot is saved in a pdf file (the default is FALSE).
#' @param path A character string for saving plot path folder .If "save" is TRUE and "path" is missing, the plot is saved in the current directory.
#' @examples
#' @import ggplot2, ggforce
#' @export
PlotWithinnessBetweenness <- function(ClustCurve,MeanCurves,save=TRUE,path=NULL)
{

  K <- length(unique(ClustCurve[,4]))
  ClustSymbol <- cluster.symbol(K)
  nfeature <- length(unique(ClustCurve[,5]))[1]
  feature.palette <- rainbow(nfeature+3)
  feature.lev <- sort(unique(ClustCurve[,5]))
  within.all <- Withinness(ClustCurve,MeanCurves,centroids=TRUE)
  between.all <- Betweenness(ClustCurve,MeanCurves,centroids=TRUE)$Betweenness
  linked <- cbind(ClustSymbol,NearestClust=between.all[,2])
  linked <- t(apply(linked,1,sort))
  betweenness <- between.all[duplicated(linked)==FALSE,]
  shift <- c(0,betweenness[,1])
  i <- rep(1,K)

  for(j in 1:(K-1))
  {
   if(!(ClustSymbol[i[j]] %in% rownames(betweenness)))
    {
    ### i[j] th cluster neighbors
    neighbors <- rownames(betweenness)[betweenness[,2]==ClustSymbol[i[j]]]
    i[j+1] <- which(ClustSymbol==neighbors[!(neighbors %in% ClustSymbol[i])])
    }
   else i[j+1] <- which(ClustSymbol==betweenness[i[j],2])
  }
  cluster.magnitudo <- apply(table(unique(ClustCurve[,c(4,1)])),1,sum)
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
  dataplot <- DataFrameWithiness.i(ClustCurve,MeanCurves,i[k],centroids=TRUE,shift=cumsum(shift)[i[k]])
  circles[index[k,],] <- dataplot$circles
  WithDist[counter:(cumsum(cluster.magnitudo)[i[k]]),] <- dataplot$WithDist
  counter <- cumsum(cluster.magnitudo)[i[k]] + 1
  }

  circles$distance <- factor(circles$distance)
  circles$Cluster <- factor(circles$Cluster)
  WithDist$Cluster <- factor(WithDist$Cluster)
  WithDist$feature <- factor(WithDist$feature)

  plots <- ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r,linetype=distance), data=circles,size=1,show.legend=FALSE)
  plots <- plots + scale_shape_manual(values=c(0:(K-1))) + geom_point(data=WithDist,aes(x=x1,y=y1,shape=Cluster,color=feature),size=2)
  feature.name <- colnames(WithDist)[4]
  plots <- plots  + geom_text(aes(x=x0, y=y0,label=Cluster), data=circles) + scale_colour_manual(values = feature.palette,name=feature.name) + labs(title="Cluster betweenness and withinness",x="distance",y="distance")
  
  if(save==TRUE)
  {
   if(is.null(path)) path <- getwd()
   ggsave(filename="Betweenness&Withinness.pdf",plot =plots,width=29, height = 20, units = "cm",scale = 1,path=path)
  }
  
  return(plots)
}

