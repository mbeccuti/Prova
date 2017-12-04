#' A glance to the dataset
#'
#' Buid data structure and plot curves
#' @param datafile .xls file for curves dataset
#' @param targetfile .txt file for curves labels
#' @return a list with info on dataset and growth curves plot
#' @examples
#' @import ggplot2
#' @export

DataGlance <- function(datafile,targetfile)
{
datasetinfo <- DataStructure(datafile,targetfile) 
p <- CurvePlot(datasetinfo)
return(datasetinfo)
}