#'
#'
#' @param
#' @param
#' @param
#' @return
#' @examples
#'
#' @import cluster.symbol
#' @export
Withness <- function(ClustCurve,MeanCurves,centroids=TRUE)
{ 
  K <- length(unique(ClustCurve[,4])) 
  ClustSymbol <- cluster.symbol(K)
  ### Withness matrix
  withness <- matrix(numeric(K*4),ncol=K)
  rownames(withness) <- c("mean","sd","min","max")
  colnames(withness) <- paste("Cluster ",ClustSymbol,sep="")

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
	### i-th cluster withness data
    withness[,i] <- cbind(MeanDist,StDev,MinDist,MaxDist)
  }
 return(withness)
}
