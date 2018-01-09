#' DataTruncation() is a function used with the purpose to truncate the data at a specific time in according
#' to the density time grid and the growth curves plot.
#'
#'
#' @param alldata List containing the number of observations per each curve (called LenCurv),
#'                and a data frame constituted
#'                from the curves' ID, observed values and the respective times.
#'                It is generated automatically from the function DataImport().
#' @param trunc.time Numeric value at which truncate curves.
#' @param feature It is the string feature name, stored in the target file.
#'                The curves will be plotted according to it.
#' @param save When TRUE (the default is FALSE), it is possible to save the growth curves plot with a vertical line
#'             at the truncation time in a pdf.
#' @param path Path to save plot to (combined with filename).
#' @return Return the list containing all the informations truncated at the time chosen, plus the feature color palette.
#' @import gridExtra, GrowthCurve
#' @export
DataTruncation <- function(alldata,trunc.time=NULL,feature,save=FALSE,path=NULL)
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
dataset <- alldata$Dataset
sample.size <- max(unique(dataset[,1]))
lencurv.tr <- numeric(sample.size)

# Data truncation
if(!is.null(trunc.time))
{
  dataset.tr <- dataset[dataset[,3]<=trunc.time,]
  for (i in 1:sample.size)  lencurv.tr[i]<-length(dataset.tr[,1][dataset.tr[,1]==i])
  timegrid.tr <- alldata$TimeGrid[alldata$TimeGrid<=trunc.time]
  alldata.tr=list(Dataset=dataset.tr,LenCurv=lencurv.tr,LabCurv=alldata$LabCurv,TimeGrid=timegrid.tr)
}
else alldata.tr <- alldata

alldata.tr$FeatureColour <- growth.curve.ls$alldata$FeatureColour

return(alldata.tr)
}
