#' User Questions
#'
#' Ask a question to the user
#'
#' @return user choice
#' @examples
#' @export
FeatureType <- function()
{
  s <- readline("Which feature to investigate cancer growth? ")
  return(as.character(s))
}
SavePlot <- function()
{
  s <- readline("Would you like to save the current plot?")
  return(as.character(s))
}