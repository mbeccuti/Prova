#' Centroid-Curves i-th cluster withinness
#'
#' Hausdorff distance between curves belonging to the i-th cluster and corresponding meancurve is computed
#'
#' @param ClustCurve A data frame with 5 arguments : time, volume, ID, cluster membership and feature values for each curves.
#' @param MeanCurves A matrix with the meancurves on the columns according to different clusters.
#' @param i a numeric value for the cluster involved in withinness computation
#' @return withmean.i a numeric vector for i-th cluster curves hausdorff distance from the corresponding meancurve
#' @examples
#' @export
WithCluster_MeanDist <- function(ClustCurve,MeanCurves,i)
{
  TimeGrid <- sort(unique(ClustCurve[,2]))
  ### i-th cluster curves data
  ClustCurve.i <- ClustCurve[ClustCurve[,4]==i,]
  ### i-th cluster curves ID
  index <- sort(unique(ClustCurve.i[,1]))
  ### i-th cluster magnitudo
  ni <- length(index)
  withmean.i <- numeric(ni)
  ### i-th cluster max obs time
  tmax <- max(ClustCurve.i[,2])
  ### i-th cluster meancurve truncated at tmax
  MeanCurve.i <- MeanCurves[,i][which(TimeGrid <= tmax)]
  ### i-th cluster obs time grid
  TimeGrid.i <- TimeGrid[TimeGrid <= tmax]
  ### i-th cluster meancurve
  A <- matrix(c(TimeGrid.i,MeanCurve.i),ncol=2)
  for (k in 1:ni)
  {
    ### i-th cluster curves
    B <- ClustCurve.i[ClustCurve.i[,1]==index[k],2:3]
    ### Hausdorff distance between i-th cluster meancurve and k-th curve respectively
    withmean.i[k] <- hausdorff(A,B)
  }
  return(withmean.i)
}

