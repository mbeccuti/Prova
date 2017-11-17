
#' @param Times A matrix with the times in the second column and the curve relative in the first one.
#' @param number_curves Number of the curves.
#' @param length Vector in which the i-component represents how many observation times the i-curve has.
#' @param save If it is "True" (default is "False") the time grid generated is saved as pdf.
#' @return The time grid plot.
#' @examples
#'
#' path <-".../1864_no_P7.csv"
#' tr.day <- 60
#' number_curves<-24
#' data <- creazionedati(path,number_curves,tr.day)
#' times<-cbind(dati$curve,dati$time)
#' length<-dati$length
#' time.grid(times,number_curves,length)
#'
#' @import
#' @export

Create.TimeGrid<-function(times,number_curves,length,save="False")
{
  coordinates<-matrix(0,nrow =length%*%length,ncol = 2)

  k<-0
  for(i in 1:number_curves)
  {
    l<-length(times[times[,1]==i,1])
    coordinates[(k+1):(k+l^2),1]<-rep(times[times[,1]==i,2],l)
    coordinates[(k+1):(k+l^2),2]<-rep(times[times[,1]==i,2],each=l)
    k<-k+l^2
  }

  Time_grid<-plot(coordinates[,1],coordinates[,2],xlab = "time",ylab = "time",main = "Time grid")


  if(save=="True")
  {
    pdf(file=paste("Time gride.pdf"),paper="a4r",width=11)
  }

  return(Time_grid)
}
