#' Growth curves
#'
#' Visualization of the line plot of the cancer growth data.
#' The curves are colored with respect of the feature chosen by the user and reported in the AnnotationFile.
#'
#' @param data CONNECTORList.
#' @param feature The column name reported in the AnnotationFile containing the feature interesting for the user to be investigated.
#' @param labels   Vector containing the text for the axis and plot title.
#' @return A list containing the line plot and the CONNECTORList.
#' @examples
#' GrowDataFile<-"data/1864dataset.xls"
#' AnnotationFile <-"data/1864info.txt"
#'
#' CONNECTORList <- DataImport(GrowDataFile,AnnotationFile)
#'
#' GrowthCurve(CONNECTORLis,"Progeny",labels=c("time","volume","Tumor Growth"))
#'
#' @import ggplot2
#' @export

GrowthCurve <- function(data,feature,labels=NULL)
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
  dataplot <- data$Dataset
  dataplot <- data.frame(merge(data$Dataset,data$LabCurv[,c("ID",feature)],by="ID"))
  dataplot[,feature]<-factor(as.matrix(dataplot[feature]))
  feature.palette <- rainbow(dim(unique(dataplot[feature]))[1])

  ### Set growth curve plot with ggplot
  GrowthCurve <- ggplot(data=dataplot, aes(x=Time, y=Vol,group=ID,col=dataplot[,feature])) +
  geom_line() +
  geom_point() +
  labs(title=title,x=axes.x, y = axes.y,col=feature)+
  theme(plot.title = element_text(hjust = 0.5),title =element_text(size=10, face='bold'))
  GrowthCurve + scale_colour_manual(values = feature.palette)

  ### Enrich data with colour palette
  data$FeatureColour <- feature.palette

  GrowthCurve.ls <- list(GrowthCurve_plot=GrowthCurve,data=data)

  return( GrowthCurve.ls )
}
