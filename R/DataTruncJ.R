#' Truncated tumor growth curves dataset
#'
#' List of information on the truncated tumor growth curves
#'
#' @param alldata A list with 4 arguments : the DataStruncture() output list.
#' @param truncTime An integer corresponding to the time at which truncate the curves. (Create.TimeGrid() output suggests suitable candidate)
#' @return A list of 4 arguments: equivalent to DataStructure() output list for truncated curve dataset.
#' @examples
#' @export
DataTrunc <- function(alldata,truncTime=NULL)
{  
   # Variables inizialization
   dataset <- alldata$Dataset
   sample.size <- max(unique(dataset[,1]))
   lencurv.tr <- numeric(sample.size)
   
   # Data truncation
  if(!is.null(truncTime))
  {
   dataset.tr <- dataset[dataset[,3]<=truncTime,]
   for (i in 1:sample.size)  lencurv.tr[i]<-length(dataset.tr[,1][dataset.tr[,1]==i])
   timegrid.tr <- alldata$TimeGrid[alldata$TimeGrid<=truncTime]
   alldata.tr=list(Dataset=dataset.tr,LenCurv=lencurv.tr,LabCurv=alldata$LabCurv,TimeGrid=timegrid.tr)
  }
  else alldata.tr <- alldata
  
  return(alldata.tr)
}

