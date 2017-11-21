#' Tumor growth curves dataset
#'
#' List of information on the tumor growth curves
#'
#' @param data A data frame with 3 variables for each curve : ID, volume and time measures.
#' @param details A data frame with 1 variable for each curve: curve name or treatment.
#' @param truncTime A time at which truncate the curves (Create.TimeGrid output suggests a suitable candidate)
#' @return A list of 3 arguments: the matrix of truncated data, the vector for details available for each curve and the vector of curves lengths
#' @examples
#' @export
DataInfo <- function(data,details,truncTime=NULL)
{
  if(!is.null(truncDay)) dataset <- data[data[,3]<=truncTime,] else dataset <- as.matrix(data)
  sample.size <- max(unique(dataset[,1]))
  lengths <- numeric(sample.size)
  for (i in 1:sample.size) lengths[i]<-length(dataset[,1][dataset[,1]==i])
  if(!is.null(details)) database <- list(data.matrix=dataset,labels=cbind(ID=unique(data[,1]),details),lencurves=lengths)
  return(database)
}





