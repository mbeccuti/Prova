#' Tumor growth curves
#'
#' Plot tumor growth curves
#'
#' @param alldata list DataStructure() output
#' @param feature string for feature name to plot curves according to
#' @param save logic save equal to TRUE save growth curves in a pdf file (default save=FALSE)
#' @param path string path for saving pdf file if save equal to TRUE (default path=NULL)
#' @return plot growth curves and enriches DataStructure() output with feature colour palette
#' @examples
#' @import ggplot2
#' @export

GrowthCurve <- function(alldata,feature,save=FALSE,path=NULL)
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
  
  ### Save plot
  if(save==TRUE)
  {
   if(!is.null(path)) ggsave(file = paste(path,"GrowthCurve.pdf",sep=""),paper="a4r",width=11)
   else print("set a path for saving growth curves plot")
  }
  
  ### Enrich alldata with colour palette
  alldata$PalColour <- feature.palette
  
  return(alldata)
}
