#' i-th cluster dataframe withinness
#'
#' Create a dataframe for i-th cluster withinness measures
#'
#' @param ClustCurve A data frame with 5 arguments : time, volume, ID, cluster membership and feature values for each curves.
#' @param MeanCurves A matrix with the meancurves on the columns according to different clusters.
#' @param i A numerical value for the cluster involved in withinness computation
#' @param shift A numerical value at which center withinness circles. 
#' @return DataFrame.i A list with 2 dataframe arguments: "circles" contains coordinates and radius for i-th cluster withinness circles measures;  
#'                                                        "WithDist" contains i-th cluster curves withinness distance from i-th cluster centroid/meancurve
#' @examples
#' @export
DataFrameWithinness.i <- function(ClustCurve,MeanCurves,i,shift=0)
{
  K <- length(unique(ClustCurve[,4]))
  ClustSymbol <- cluster.symbol(K)
  feature <- colnames(ClustCurve)[5]
  nfeature <- length(unique(ClustCurve[,feature]))[1]
  featurecurve.i <- t(unique(ClustCurve[ClustCurve[,4]==i,c(1,5)])[2])
  
  ### Centroid withinness distance
  Withinness.i <- WithCluster_MeanDist(ClustCurve,MeanCurves,i)
  
  ### Mean and standard deviation distance
  mean.dist <- mean(Withinness.i)
  st.dev <- sd(Withinness.i)

  ### Data frame for ggplot
  circles.i <- data.frame(
  x0 <- rep(0+shift,3),
  y0 <- rep(0,3),
  r <- rep(mean.dist, 3)+c(st.dev,0,-st.dev),
  distance <- c("sd","mean","sd"),
  Cluster <- factor(ClustSymbol[i],levels=ClustSymbol)
  )
  colnames(circles.i) <- c("x0","y0","r","distance","Cluster")

  WithDist.i <- data.frame(
  x1 <- Withinness.i + shift,
  y1 <- numeric(length(Withinness.i)),
  Cluster <- factor((i-1),levels=c(0:(K-1))),
  type <- factor(featurecurve.i,levels=c(1:nfeature))
  )
  colnames(WithDist.i) <- c("x1","y1","Cluster","feature")
 return(DataFrame.i=list(circles=circles.i,WithDist=WithDist.i))
 }



