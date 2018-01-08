#' Data Truncation
#'
#' Truncate curve dataset
#'
#' @param alldata list DataImport() output
#' @param trunc.time numeric value at which truncate curves
#' @return alldata list DataImport() output with feature color palette
#' @import gridExtra
#' @export
DataTruncation <- function(alldata,trunc.time,feature,save=FALSE,path=NULL)
{
### Variables initialization
growth.curve.ls <- GrowthCurve(alldata,feature)
### Plot growth curves with truncation time
growth.curve.tr <- growth.curve.ls$GrowthCurve_plot + geom_vline(xintercept=trunc.time, color="black", size=1)

if(save==TRUE)
{
  ggsave(filename="DataTruncation.pdf",plot =growth.curve.tr,width=29, height = 20, units = "cm",scale = 1,path=path )
}



### Truncated dataset
alldata.tr <- DataTrunc(alldata,truncTime=trunc.time)
alldata.tr$FeatureColour <- growth.curve.ls$alldata$FeatureColour

return(alldata.tr)
}
