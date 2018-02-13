#' Growth curves
#'
#' Plots the sample curves.
#'
#' @param alldata list DataImport() output
#' @param feature string feature name to plot curves according to
#' @param labels  The text for the axis and plot title.
#' @return GrowthCurve.ls list with growth curves plot and DataImport() output enriched by feature colour palette
#' @examples
#' @import ggplot2
#' @export

GrowthCurve <- function(alldata,feature,labels=NULL)
{
  if(is.null(labels))
  {
    axes.x<-""
    axes.y<-""
    title<-""

  }else{
    axes.x<-labels[1]
    axes.y<-labels[2]
    title<-labels[3]
  }


  ### dataframe for ggplot
  dataplot <- alldata$Dataset
  dataplot <- data.frame(merge(alldata$Dataset,alldata$LabCurv[,c("ID",feature)],by="ID"))
  dataplot[,feature]<-factor(as.matrix(dataplot[feature]))
  feature.palette <- rainbow(dim(unique(dataplot[feature]))[1])

  ### Set growth curve plot with ggplot
  GrowthCurve <- ggplot(data=dataplot, aes(x=Time, y=Vol,group=ID,col=dataplot[,feature])) +
  geom_line() +
  geom_point() +
  labs(title=title,x=axes.x, y = axes.y,col=feature)+
  theme(plot.title = element_text(hjust = 0.5),title =element_text(size=10, face='bold'))
  GrowthCurve + scale_colour_manual(values = feature.palette)

  ### Enrich alldata with colour palette
  alldata$FeatureColour <- feature.palette

  GrowthCurve.ls <- list(GrowthCurve_plot=GrowthCurve,alldata=alldata)

  return( GrowthCurve.ls )
}
