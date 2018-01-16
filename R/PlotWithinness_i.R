#' i-th cluster withinness plot
#'
#' Plot i-th cluster withinness measure circles 
#'
#' @param ClustCurve A data frame with 5 arguments : time, volume, ID, cluster membership and feature values for each curves.
#' @param MeanCurves A matrix with the meancurves on the columns according to different clusters.
#' @param i a numerical value for the cluster involved in withinness computation
#' @param centroids A logical value for specifying how to compute withinness. If "centroids" equals TRUE (default value), WithCluster_MeanDist() function is used, otherwise withinness is calculated using WithCluster_CurvDist().
#' @param shift A numerical value at which center withinness circles. 
#' @return plot.i A ggplot() object for i-th cluster withinness measures circles
#' @examples
#' @import ggforce
#' @export

PlotWithinness.i <- function(ClustCurve,MeanCurves,i,shift=0)
{
  dataplot <- DataFrameWithinness.i(ClustCurve,MeanCurves,i,shift=shift)
  ### Data frame for plot
  circles <- dataplot$circles
  WithDist <- dataplot$WithDist
  feature <- WithDist[,3]
  feature.name <- colnames(WithDist)[3]
  nfeature <- length(unique(ClustCurve[,feature.name]))[1]
  feature.palette <- rainbow(nfeature)
  ### Plot distances
  plot.i <- ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r), data=circles,size=1)
  plot.i <- plot.i  + geom_point(data=WithDist,aes(x=x1,y=y1,col=feature),shape=(i-1)) + labs(title=paste("Cluster",ClustSymbol[i],"withinness"),x="distance",y="distance")
  plot.i <- plot.i + scale_colour_manual(values = feature.palette[unique(feature)],name=feature.name) + xlab("distance x") + ylab("distance y")
  return(plot.i)
 }