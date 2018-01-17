#' i-th cluster curves withinness
#'
#' Hausdorff distance between curves belonging to the i-th cluster 
#'
#' @param ClustCurve A data frame with 5 arguments : time, volume, ID, cluster membership and feature values for each curves.
#' @param i a numeric value for the cluster involved in withinness computation
#' @return within.i a numeric vector for i-th cluster curves relative hausdorff distance 
#' @examples
#' @export
WithCluster_CurvDist <- function(ClustCurve,i)
{

  ### i-th cluster curves data
  ClustCurve.i <- ClustCurve[ClustCurve[,4]==i,]
  ### i-th cluster curves ID
  index <- sort(unique(ClustCurve.i[,1]))
  ### i-th cluster magnitudo
  ni <- length(index)

  if(ni!=1)
   {
   within.i <- numeric((1/2)*(factorial(ni)/(factorial(ni-2))))
   count <- 0
   for (k in 1:(ni-1))
    {
      A <- ClustCurve.i[ClustCurve.i[,1]==index[k],2:3]
      for (j in (k+1):ni)
       {
         B <- ClustCurve.i[ClustCurve.i[,1]==index[j],2:3]
         count <- count +1
         within.i[count] <- hausdorff(A,B)
       }
    }
   }
  else within.i <- 0
  ### i-th cluster withinness
  return(within.i)
}

