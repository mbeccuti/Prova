#' Observations density across time
#'
#' Plot time grid density
#' @param  alldata list DataImport() output
#' @return time grid density plot
#' @examples
#' @import ggplot2
#' @export
TimeGridDensity <- function(alldata)
{
 ### library
 library(ggplot2)
 
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
      geom_point(aes(Time1, Time2, col = d), size = 4) +
	  coord_fixed(ratio = 1) +
      scale_color_identity() +
      theme_bw() +
	  labs(title="Time grid",x="Time", y = "Time") 
 
  return(TimeGrid_plot)
  
}