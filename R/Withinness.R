#' Overall Clusters withinness
#'
#' Withinness across all clusters is computed
#'
#' @param ClustCurve A data frame with 5 arguments : time, volume, ID, cluster membership and feature values for each curves.
#' @param MeanCurves A matrix with the meancurves on the columns according to different clusters.
#' @param centroids A logical value for specifying how to compute withinness. If "centroids" equals TRUE (default value), WithCluster_MeanDist() function is used, otherwise withinness is calculated using WithCluster_CurvDist().
#' @return withinness a numeric matrix with 4 columns: mean, standard deviation, minimum and maximum withinness distance across distinct clusters.
#' @examples
#' @export
Withinness <- function(ClustCurve,MeanCurves,centroids=TRUE)
{
  K <- length(unique(ClustCurve[,4]))
  ClustSymbol <- cluster.symbol(K)
  ### Withinness matrix
  withinness <- matrix(numeric(K*4),ncol=K)
  rownames(withinness) <- c("mean","sd","min","max")
  colnames(withinness) <- paste("Cluster ",ClustSymbol,sep="")

  for (i in 1:K)
  {
    if(centroids==FALSE) ### i-th cluster curves distance
	{
	  within.i <- WithCluster_CurvDist(ClustCurve,i)
	}
    else                 ### i-th cluster curves and meancurve distance
	  within.i <- WithCluster_MeanDist(ClustCurve,MeanCurves,i)

    ### i-th cluster mean distance
    MeanDist <- mean(within.i)
	### i-th cluster standard deviation distance
    if(length(within.i)==1) { StDev <- 0 }
    else                        StDev <- sd(within.i)
	MinDist <- min(within.i)
	MaxDist <- max(within.i)
	### i-th cluster withinness data
    withinness[,i] <- cbind(MeanDist,StDev,MinDist,MaxDist)
  }
 return(withinness)
}
