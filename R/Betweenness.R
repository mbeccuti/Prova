#' Overall clusters betweenness
#'
#' Betweenness across all clusters is computed
#'
#' @param ClustCurve A data frame with 5 arguments : time, volume, ID, cluster membership and feature values for each curves.
#' @param MeanCurves A matrix with the meancurves on the columns according to different clusters.
#' @param centroids A logical value for specifying how to compute betweenness. If "centroids" equals TRUE (default value), BetweenCluster_MeanDist() function is used, otherwise withinness is calculated using BetweenCluster_CurvDist().
#' @return A list with 4 arguments (if "centroids" equals TRUE): Betweenness is a matrix with 2 columns, the minimum cluster distance and the cluster name that achieves the minimum;
#'                                                               CentroidDist is a K x K matrix (where K is the cluster number) containing centroids cluster distance among each cluster couple;
#'                                                               Classes a vector containing the cluster name membership for each dataset curve.
#'         A list with 3 arguments (if "centroids" equals FALSE): NearDist is a matrix containing, for each cluster, the nearest curve distance, ID and cluster membership across all clusters ;
#'                                                                FarDist is a matrix containing, for each cluster, the farthest curve distance, ID and cluster membership across all clusters;
#'                                                                Classes a vector containing the cluster name membership for each dataset curve.
#' @examples
#' @export
Betweenness <- function(ClustCurve,MeanCurves,centroids=TRUE)
{
  K <- length(unique(ClustCurve[,4]))
  ClustSymbol <- cluster.symbol(K)
  ClassCurve <- unique(ClustCurve[,c(1,4)])[,2]
  classes <- ClustSymbol[ClassCurve]
  if(centroids==FALSE)
  {
   between.near <- matrix(numeric(K*3*(K-1)),ncol=3*(K-1))
   rownames(between.near) <- paste("cluster",ClustSymbol)
   colnames(between.near) <- rep(c("Distance","ID","cluster"),3)
   between.far <- between.near
   for (i in 1:K)
    {
	 Betweenness.i <- BetweenCluster_CurvDist(ClustCurve,i)
	 near.curve <- Betweenness.i$NearCurve
     far.curve <- Betweenness.i$FarCurve
	 count <- 1
	 index <- seq(1,3*(K-1),3)
	 for (j in index)
	 {
	 between.near[i,j[count]:(j[count]+2)] <- c(t(Betweenness.i$Between[,1]),near.curve,ClustSymbol[ClassCurve[near.curve]])
	 between.far[i,j[count]:(j[count]+2)] <- c(t(Betweenness.i$Between[,2]),far.curve,ClustSymbol[ClassCurve[far.curve]])
	 }
	 count <- count + 1
	 }
	 
   return(list(NearDist=between.near,FarDist=between.far,Classes=classes))
  }

  else
  {
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
}
