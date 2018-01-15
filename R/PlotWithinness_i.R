PlotWithinness.i <- function(ClustCurve,MeanCurves,i,centroids=TRUE,shift=0)
{
  library(ggforce)
  dataplot <- DataFrameWithinness.i(ClustCurve,MeanCurves,i,centroids=centroids,shift=shift)
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