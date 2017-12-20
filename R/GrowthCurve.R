#' Tumor growth curves
#'
#' Plot tumor growth curves
#'
#' @param alldata list DataImport() output
#' @param feature string feature name to plot curves according to
#' @return GrowthCurve.ls list with growth curves plot and DataImport() output enriched by feature colour palette
#' @examples
#' @import ggplot2
#' @export

GrowthCurve <- function(alldata,feature)
{ 
  ### library
  library(ggplot2)
  
  ### dataframe for ggplot
  dataplot <- alldata$Dataset
  dataplot <- data.frame(merge(alldata$Dataset,alldata$LabCurv[,c("ID",feature)],by="ID"))
  dataplot[,feature]<-factor(as.matrix(dataplot[feature])) 
  feature.palette <- rainbow(dim(unique(dataplot[feature]))[1])
  
  ### Set growth curve plot with ggplot
  GrowthCurve <- ggplot(data=dataplot, aes(x=Time, y=Vol,group=ID,col=dataplot[,feature])) +
  geom_line() +
  geom_point() +
  labs(title="Tumor growth curves",x="Time", y = "Volume",col=feature) 
  GrowthCurve + scale_colour_manual(values = feature.palette)
  
  ### Enrich alldata with colour palette
  alldata$PalColour <- feature.palette
  
  GrowthCurve.ls <- list(GrowthCurve_plot=GrowthCurve,alldata=alldata)
  
  return( GrowthCurve.ls )
}
