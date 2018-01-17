#' DataTruncation() is a function used with the purpose to truncate the data at a specific time in according
#' to the density time grid and the growth curves plot.
#'
#'
#' @param alldata A list with 4 arguments : the DataStruncture() output list.
#' @param truncTime An integer corresponding to the time at which truncate the curves. (TimeGridDensity() output suggests suitable candidates).
#' @param feature A character string for a target file feature name to investigate across curves.
#' @param save A logical value. If "save" equals TRUE truncated growth curves plot is saved in a pdf file (the default is FALSE).
#' @param path A character string for saving plot path folder .If "save" is TRUE and "path" is missing, the plot is saved in the current directory.
#' @return The growth curves plot with a bar at the truncation time and a list with 4 arguments: a data frame with 3 variables (ID curves, volume and time values
#'         truncated at the truncation time), a vector for truncated curves lengths,a data frame with curves labeled according to target file feature chosen and a vector for overall truncated time grid.
#'         If "save" equals TRUE, a pdf file containing the plot is returned too.
#' @export
DataTruncation <- function(alldata,feature,truncTime=NULL,save=FALSE,path=NULL)
{
### Variables initialization
growth.curve.ls <- GrowthCurve(alldata,feature)
### Plot growth curves with truncation time
growth.curve.tr <- growth.curve.ls$GrowthCurve_plot 
if(! is.null(truncTime)) growth.curve.tr <- growth.curve.tr + geom_vline(xintercept=truncTime, color="black", size=1)

if(save==TRUE)
{
 if(is.null(path)) 
 {
  path <- getwd()
  ggsave(filename="DataTruncation.pdf",plot =growth.curve.tr,width=29, height = 20, units = "cm",scale = 1,path=path )
 }
}

### Truncated dataset
dataset <- alldata$Dataset
sample.size <- max(unique(dataset[,1]))
lencurv.tr <- numeric(sample.size)

# Data truncation
if(!is.null(truncTime)) alldata.tr <- DataTrunc(alldata,truncTime=truncTime)
else alldata.tr <- alldata
 
alldata.tr$LabCurv <- alldata.tr$LabCurv[c("ID",feature)]

plot(growth.curve.tr)

return(alldata.tr)
}
