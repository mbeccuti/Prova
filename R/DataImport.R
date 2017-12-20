#' Data structure for package name
#'
#' Create curve dataset
#'
#' @param file1 .xls file name with volume and time values
#' @param file2 .txt file name with curve labels
#' @return A list with 4 arguments: a data frame with 3 variables (ID curves, volume and times value), a vector for curves lengths, a data frame with curves labeled according to target file features and a vector for overall time grid.
#' @examples
#' @import readxl
#' @export
DataImport <- function(file1,file2) {

  ### library
  library(readxl)
  
  ###Read Data File
  dataset <- read_xls(file1,col_names=T)

  ### Inizialize :
  ### vector for curves lenghts
  nvar       <- dim(dataset)[2]
  nobs       <- dim(dataset)[1]
  tot        <- nobs*nvar
  samplesize <- dim(dataset)[2]/2
  lencurv    <- numeric(samplesize)
  ### vectors for times, volume and ID curves values
  tot        <- length(dataset)
  TimeIndex  <- seq(1,nvar,2)
  VolIndex   <- seq(2,nvar,2)
  TimeValue  <- as.matrix(dataset[TimeIndex])
  VolValue   <- as.matrix(dataset[VolIndex])

  times      <- numeric(tot)
  vol        <- numeric(tot)
  ID         <- numeric(tot)

  ### Organize times, volume and ID curves values, removing NA
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
      ID[1:lencurv[cappa]] <- cappa
    }

    else
    {
      lcum <- cumsum(lencurv)
      vol[(lcum[cappa-1]+1):lcum[cappa]] <- tempv
      times[(lcum[cappa-1]+1):lcum[cappa]] <- tempt
      ID[(lcum[cappa-1]+1):lcum[cappa]] <- cappa
    }
  }

  ndata    <- sum(lencurv) 
  vol      <- vol[1:ndata]
  times    <- times[1:ndata]
  ID       <- ID[1:ndata]
  timegrid <- 1:max(times)

  ### Read Target File
  labcurv  <- read.csv(file=file2,header=TRUE)

  ### ID, volume and time data frame

  dataset <- data.frame(ID=ID,Vol=vol,Time=times)
  alldata <- list(Dataset=dataset,LenCurv=lencurv,LabCurv=labcurv,TimeGrid=timegrid)


  return(alldata)
}
