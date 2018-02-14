
#' Overall clusters betweenness
#'
#' Betweenness across all clusters is computed
#'
#' @param ClustCurve A data frame with 5 arguments : time, volume, ID, cluster membership and feature values for each curves.
#' @param MeanCurves A matrix with the meancurves on the columns according to different clusters.
#' @return A list with 4 arguments (if "centroids" equals TRUE): Betweenness is a matrix with 2 columns, the minimum cluster distance and the cluster name that achieves the minimum;
#'CentroidDist is a K x K matrix (where K is the cluster number) containing centroids cluster distance among each cluster couple;
#'Classes a vector containing the cluster name membership for each dataset curve.
#' @examples
#' @export
Betweenness <- function(ClustCurve,MeanCurves)
{
  K <- length(unique(ClustCurve[,4]))
  ClustSymbol <- cluster.symbol(K)
  ClassCurve <- unique(ClustCurve[,c(1,4)])[,2]
  classes <- ClustSymbol[ClassCurve]
  centroid.dist <- matrix(numeric(K*K),nrow=K)
  between <- matrix(numeric(K*2),ncol=2)
  colnames(between) <- c("Min distance","Nearest cluster")
  rownames(between) <- ClustSymbol
  rownames(centroid.dist) <- ClustSymbol
  colnames(centroid.dist) <- rownames(centroid.dist)

  for(i in 1:K)
    {
	centroid.dist[i,] <-  BetweenCluster_MeanDist(ClustCurve,MeanCurves,i)
	ClustSymbol.i <- ClustSymbol[-i]
	min.dist <- min(centroid.dist[i,][centroid.dist[i,]!=0])
	argmin.dist <- ClustSymbol.i[which.min(centroid.dist[i,][centroid.dist[i,]!=0])]
	between[i,] <- c(min.dist,argmin.dist)
	}
 return(list(Betweenness=between,CentroidDist=centroid.dist,Classes=classes))
}
