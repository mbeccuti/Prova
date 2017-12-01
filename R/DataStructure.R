#' Data structure for package name
#'
#' Create curve dataset
#'
#' @param file1 .xls file name with volume and time values
#' @param file2 .txt file name with curve labels
#' @return A list with 3 arguments: a matrix with 3 columns (ID curves, volume and times value), curves lengths and the grid of overall observation times.
#' @examples
#' @export
DataStructure <- function(file1,file2) {

  #Read file with times and volume data
  library(readxl)

  dataset <- read_xls("1864_dataset.xls",col_names=T)

  # Inizialize :
  # vector for curves lenghts
  nvar <- dim(dataset)[2]
  nobs <- dim(dataset)[1]
  tot <- nobs*nvar
  samplesize <- dim(dataset)[2]/2
  lencurv <- numeric(samplesize)
  # vectors for times, volume and ID curves values
  tot <- length(dataset)
  TimeIndex <- seq(1,nvar,2)
  VolIndex  <- seq(2,nvar,2)
  TimeValue   <- as.matrix(dataset[TimeIndex])
  VolValue <- as.matrix(dataset[VolIndex])

  times <- numeric(tot)
  vol <- numeric(tot)
  curves <- numeric(tot)

  # Organize times, volume and ID curves values, removing NA
  for (cappa in 1:samplesize)
  {
    tempv <- VolValue[,cappa]
    tempv <- tempv[!is.na(tempv)]
    lencurv[cappa] <- length(tempv)
    tempt <-TimeValue[,cappa]
    tempt <-tempt[!is.na(tempt)]

    if (cappa == 1)
    {
      vol[1:lencurv[cappa]]    <- tempv
      times[1:lencurv[cappa]]  <- tempt
      curves[1:lencurv[cappa]] <- cappa
    }

    else
    {
      lcum <- cumsum(lencurv)
      vol[(lcum[cappa-1]+1):lcum[cappa]] <- tempv
      times[(lcum[cappa-1]+1):lcum[cappa]] <- tempt
      curves[(lcum[cappa-1]+1):lcum[cappa]] <- cappa
    }
  }

  ndata    <- sum(lencurv) # effective number of overall observation without NA
  vol      <- vol[1:ndata]
  times    <- times[1:ndata]
  curves   <- curves[1:ndata]
  timegrid <- 1:max(times)

  # ID, volume and time matrix
  mdata    <- cbind(curves,vol,times)
  colnames(mdata) <- c("ID","Vol","Time")

  #
  labcurv  <- read.csv(file=file2,header=TRUE)

  return(dataset=list(data.matrix=mdata,LenCurv=lencurv,Labels=labcurv,TimeGrid=timegrid))
}
