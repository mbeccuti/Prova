#' Observations density across time
#'
#' Plot time grid density
#' @param  dataset list, DataStructure() output
#' @return time grid density plot
#' @examples
#' @export
DensityGrid <- function(dataset)
{
TimeMeasure <-dataset$mydata[,2:3]
SampleSize <- length(unique(dataset$mydata[,2]))
LenCurve <- dataset$LenCurv
PointsCoord<-matrix(0,nrow =LenCurve%*%LenCurve,ncol = 2)

k<-0
for(i in 1:SampleSize)
{
  l<-length(TimeMeasure[TimeMeasure[,1]==i,1])
  PointsCoord[(k+1):(k+l^2),1]<-rep(TimeMeasure[TimeMeasure[,1]==i,2],l)
  PointsCoord[(k+1):(k+l^2),2]<-rep(TimeMeasure[TimeMeasure[,1]==i,2],each=l)
  k<-k+l^2
}
Time1 <- PointsCoord[,1]
Time2 <- PointsCoord[,2]
Points <- data.frame(PointsCoord)
x <- densCols(Time1,Time2, colramp=colorRampPalette(c("black", "white")))
Points$dens <- col2rgb(x)[1,] + 1L

cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
                            "#FCFF00", "#FF9400", "#FF3100"))(256)
Points$col <- cols[Points$dens]

windows()
plot(Time2~Time1, data=Points[order(Points$dens),], pch=20, col=col, cex=2)

}