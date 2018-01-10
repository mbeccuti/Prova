#' Time Grid Density
#'
#'@description
#' TimeGridDensity() is a convenient function for controlling if the time grid is enough dense.
#' It is possible to save the plot in a pdf specifying the path.
#'
#'
#' @param  alldata List containing the number of observations per each curve (called LenCurv), and a data frame constituted
#'                from the curves' ID, observed values and the respective times.
#'                It is generated automatically from the function DataImport().
#' @param  save When TRUE (the default is FALSE), it is possible to save the plot in a pdf.
#' @param  path	Path to save plot to (combined with filename).
#' @return The time grid density plot as a ggplot object.
#' @examples to write...
#' @import ggplot2
#' @export
TimeGridDensity <- function(alldata,save=FALSE,path=NULL)
{

 ### Variables initialization
 TimeMeasure <-alldata$Dataset[,c(1,3)]
 SampleSize <- length(unique(alldata$Dataset[,1]))
 LenCurve <- alldata$LenCurv
 PointsCoord<-matrix(0,nrow =LenCurve%*%LenCurve,ncol = 2)
 k<-0

 # Generate time grid
  for(i in 1:SampleSize)
  {
    l<-length(TimeMeasure[TimeMeasure[,1]==i,1])
    PointsCoord[(k+1):(k+l^2),1]<-rep(TimeMeasure[TimeMeasure[,1]==i,2],l)
    PointsCoord[(k+1):(k+l^2),2]<-rep(TimeMeasure[TimeMeasure[,1]==i,2],each=l)
    k<-k+l^2
  }

  ### Generate data frame for ggplot
  Time1 <- PointsCoord[,1]
  Time2 <- PointsCoord[,2]
  df <- data.frame(Time1=Time1,Time2=Time2,
  d = densCols(Time1, Time2, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

  ### Plot density grid
  TimeGrid_plot <- ggplot(df) +
      geom_point(aes(Time1, Time2), size = 2) +
	    coord_fixed(ratio = 1) +
      #scale_color_identity() +
      theme_bw() +
	    labs(title="Time grid",x="Time", y = "Time")+
      theme(plot.title = element_text(hjust = 0.5),title =element_text(size=12, face='bold'))

  if(save==TRUE)
  {
    ggsave(filename="TimeGrid.pdf",plot =TimeGrid_plot,width=29, height = 20, units = "cm",scale = 1,path=path )
  }
  return(TimeGrid_plot)

}
