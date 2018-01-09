#' DataTruncation() is a function used with the purpose to truncate the data at a specific time in according
#' to the density time grid and the growth curves plot.
#'
#'
#' @param alldata List containing the number of observations per each curve (called LenCurv),
#'                and a data frame constituted
#'                from the curves' ID, observed values and the respective times.
#'                It is generated automatically from the function DataImport().
#' @param trunc.time Numeric value at which truncate curves.
#' @param feature The growth curve plot will be colored with respect of the feature,
#'                a qualitative information stored in the target file.
#' @param save When TRUE (the default is FALSE), it is possible to save the growth curves plot with a vertical line
#'             at the truncation time in a pdf.
#' @param path Path to save plot to (combined with filename).
#' @return Return the list containing all the informations truncated at the time chosen, plus the feature color palette.
#' @import gridExtra, GrowthCurve, DataTrunc
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
