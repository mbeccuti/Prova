#' Counting samples
#'
#'@description
#'
#'
#' @param cluster
#' @param Model
#' @param
#' @return
#' @examples
#'
#'
#' @import plyr
#' @export
Counting<-function(cluster,Model=NULL)
{

 if(is.null(Model))
 {
   Model<-c("FCM","Malthus","Gompertz","Logistic")
 }


 counting <-list()

 for( i in Model)
 {
   ClustCurve<-cluster[[i]]$Information$ClustCurve
   feature<-tail(colnames(ClustCurve),1)
   a<-count(ClustCurve, c("ID", "Cluster",feature))[,-4]
   counting[[paste(i)]]<-count(a,c( "Cluster",feature))
 }

}
