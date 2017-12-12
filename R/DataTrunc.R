#' Truncated tumor growth curves dataset
#'
#' List of information on the truncated tumor growth curves
#'
#' @param data A list with 4 arguments : the DataStruncture() output list.
#' @param truncTime An integer corresponding to the time at which truncate the curves. (Create.TimeGrid() output suggests suitable candidate)
#' @return A list of 2 arguments: the matrix of truncated data and the vector of truncated curves lengths
#' @examples
#' @export
DataTrunc <- function(dataset,truncTime=NULL)
{  mdata <- dataset$data.matrix
   sample.size <- max(unique(mdata[,1]))
   lengths.tr <- numeric(sample.size)
  if(!is.null(truncTime))
  {
   mdata.tr <- mdata[mdata[,3]<=truncTime,]
   for (i in 1:sample.size) lengths.tr[i]<-length(mdata.tr[,1][mdata.tr[,1]==i])
  }

  return(Data.tr=list(data.matrixtr=mdata.tr,LenCurv.tr=lengths.tr))
}

