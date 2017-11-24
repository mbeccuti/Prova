#' Data structure for package name
#'
#' Create dataset reading .csv file with times and volume data.
#'
#' @param filename .csv file name with file path
#' @param samplesize An integer for the number of the growth curves.
#' @return A list with 3 arguments: a matrix with 3 columns (ID curves, volume and times value), curves lengths and the grid of overall observation times.
#' @examples
#' @export
DataStructure <- function(filename,samplesize) {

  #Read file with times and volume data
  dataset <- scan(file=filename,skip=1,sep=',')

  # Inizialize :
  # vector for curves lenghts
  lencurv <- numeric(samplesize)
  # vectors for times, volume and ID curves values
  tot <- length(dataset)
  TimeIndex <- seq(2*samplesize+1,tot,2)
  VolIndex  <- seq(2*samplesize+2,tot,2)
  TimeValue   <- matrix(data=dataset[TimeIndex],ncol=samplesize,byrow=TRUE)
  VolValue <- matrix(data=dataset[VolIndex],ncol=samplesize,byrow=TRUE)

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

  ndata   <- sum(lencurv) # effective number of overall observation without NA
  vol     <- vol[1:ndata]
  times   <- times[1:ndata]
  curves  <- curves[1:ndata]
  timegrid <- 1:max(times)

  mdata <- cbind(curves,vol,times)
  colnames(mdata) <- c("ID","Vol","Time")

  return(dataset=list(data.matrix=mdata,LenCurv=lencurv,TimeGrid=timegrid))
}
