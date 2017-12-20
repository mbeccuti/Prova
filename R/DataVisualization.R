#' Data Visualization
#'
#' Create curve dataset
#'
#' @param alldata list DataImport() output
#' @param feature string feature name to plot curves according to
#' @return alldata list DataImport() output with feature color palette
#' @import gridExtra
#' @export
DataVisualization <- function(alldata,feature,save=FALSE,path=NULL)
{
 ### library
 library(gridExtra)
 
 ### Variables initialization
 growth.curves <- GrowthCurve(alldata,feature)
 plot1 <- growth.curves$GrowthCurve_plot
 plot2 <- TimeGridDensity(alldata)
 
 if(save==TRUE) 
 {
   if(!is.null(path)) 
    { 
    pdf(paste(path,"DataVisualization.pdf",sep=""),paper="a4r",width=11)
	#multiplot(plot1,plot2,ncols=2)
	grid.arrange(plot1 , plot2 , ncol=2)
	dev.off()
    }
  else print("set a path for saving plots")
 }
 return(alldata=growth.curves$alldata)
}

