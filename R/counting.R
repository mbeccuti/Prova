#' Counting samples
#'
#'@description
#'
#'
#' @param cluster
#' @param Model
#' @return
#' @examples
#'
#'
#' @import plyr
#' @export
CountingSamples<-function(cluster,Model=NULL)
{

 if(is.null(Model))
 {
   Model<-c("FCM","Malthus","Gompertz","Logistic")
 }

 Counting<-list()

 for( i in Model)
 {
   ClustCurve<-cluster[[i]]$Information$ClustCurve
   feature<-tail(colnames(ClustCurve),1)
   a<-count(ClustCurve, c("ID", "Cluster",feature))[,-4]
   Counting[[paste(i)]]<-count(a,c( "Cluster",feature))
 }

 return(Counting)
}
