#' Data structure for package name
#'
#' Create curve dataset
#'
#' @param file1 .xls file name with volume and time values
#' @param file2 .txt file name with curve labels
#' @return A list with 3 arguments: a matrix with 3 columns (ID curves, volume and times value), curves lengths and the grid of overall observation times.
#' @examples
#' @import readxl
#' @export 
DataStructure <- function(file1,file2) {

  #Read Data File
  library(readxl)

  dataset <- read_xls(file1,col_names=T)

  # Inizialize :
  # vector for curves lenghts
  nvar       <- dim(dataset)[2]
  nobs       <- dim(dataset)[1]
  tot        <- nobs*nvar
  samplesize <- dim(dataset)[2]/2
  lencurv    <- numeric(samplesize)
  # vectors for times, volume and ID curves values
  tot        <- length(dataset)
  TimeIndex  <- seq(1,nvar,2)
  VolIndex   <- seq(2,nvar,2)
  TimeValue  <- as.matrix(dataset[TimeIndex])
  VolValue   <- as.matrix(dataset[VolIndex])

  times      <- numeric(tot)
  vol        <- numeric(tot)
  ID         <- numeric(tot)

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

  ndata    <- sum(lencurv) # effective number of overall observation without NA
  vol      <- vol[1:ndata]
  times    <- times[1:ndata]
  ID       <- ID[1:ndata]
  timegrid <- 1:max(times)

  # Read Target File
  labcurv  <- read.csv(file=file2,header=TRUE)
  
    # Ask use to feature for investigation
  featUserChoice <- FeatureType()
  if (featUserChoice!="none") feature <- labcurv[,featUserChoice] else feature<- NULL

  # # Inizialize variables for plot
  # labgroup    <- unique(feature)
  # maxvol      <- max(vol)
  # maxtimes    <- max(times)
  nfeature    <- length(unique(feature))
  if(!is.null(feature)) colour <- palette(rainbow(nfeature)) else colour <- "black"

  # # PLOT GROWTH CURVES
# tit         <- c(" GrowthCurve ")
# file.name   <- paste(tit,".pdf")

# #pdf(file =file.name, paper = "a4r", width = 11)

   # # plot 1^st curve
   # windows()
   # x <- times[ID==1]
   # y <- vol[ID==1]
   # cl <- ifelse(!is.null(feature),colour[feature[1]],colour[1])
   # plot(x,,col=cl,type="l",xlab='Times',ylab='Volume',cex=1.5, xlim=c(0,maxtimes), ylim=c(0,maxvol),lwd=2)
   # points(x,y, col=cl,lwd=2,pch=16)
   # # plot remaining curves
   # for (i in 2:samplesize) {
   # x <- times[ID==i]
   # y <- vol[ID==i]
   # cl <- ifelse(!is.null(feature),colour[feature[i]],colour[1])
   # lines(x,y, col=cl,cex=1.5,lwd=2)
   # points(x,y,col=cl,lwd=2,pch=16)
   # }

# # make legend and title
# #legend("topleft", labgroup, lty=1,col=colour, title="Legend", bty="n", cex=1,lwd=2)
# title(tit)
# Sys.sleep(3)
# dev.copy2pdf(device = postscript, file = "C:/Users/Jessica/Desktop/GrowthCurve.pdf",paper="a4r",width=11)
# dev.off()

   # ID, volume and time matrix
  dataset <- data.frame(Groups=rep(feature,lencurv),ID=ID,Time=times,Vol=vol)
 # mdata    <- cbind(ID,vol,times)
  # colnames(mdata) <- c("ID","Vol","Time")
  alldata <- list(mydata=dataset,LenCurv=lencurv,TimeGrid=timegrid,PalColour=colour)

  return(alldata)
}
