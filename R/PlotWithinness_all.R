PlotWithinness.all <- function(ClustCurve,MeanCurves,centroids=TRUE,shift=0)
{
  library(ggplot2)
  K <- length(unique(ClustCurve[,4]))
  ClustSymbol <- cluster.symbol(K)
  plots <- vector("list",K)
  maxdist <- max(Withinness(ClustCurve,MeanCurves,centroids)[4,])
  axislim <- c(-maxdist,maxdist)
  for(i in 1:K) plots[[i]] <- PlotWithinness.i(ClustCurve,MeanCurves,i,centroids,shift)+ coord_cartesian(xlim = axislim, ylim = axislim)
  return(plots)
}