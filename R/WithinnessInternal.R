
WithCluster_CurvDist <- function(ClustCurve,i)
{

  ### i-th cluster curves data
  ClustCurve.i <- ClustCurve[ClustCurve[,4]==i,]
  ### i-th cluster curves ID
  index <- sort(unique(ClustCurve.i[,1]))
  ### i-th cluster magnitudo
  ni <- length(index)

  if(ni!=1)
   {
   within.i <- numeric((1/2)*(factorial(ni)/(factorial(ni-2))))
   count <- 0
   for (k in 1:(ni-1))
    {
      A <- ClustCurve.i[ClustCurve.i[,1]==index[k],2:3]
      for (j in (k+1):ni)
       {
         B <- ClustCurve.i[ClustCurve.i[,1]==index[j],2:3]
         count <- count +1
         within.i[count] <- hausdorff(A,B)
       }
    }
   }
  else within.i <- 0
  ### i-th cluster withinness
  return(within.i)
}

WithCluster_MeanDist <- function(ClustCurve,MeanCurves,i)
{
  TimeGrid <- sort(unique(ClustCurve[,2]))
  ### i-th cluster curves data
  ClustCurve.i <- ClustCurve[ClustCurve[,4]==i,]
  ### i-th cluster curves ID
  index <- sort(unique(ClustCurve.i[,1]))
  ### i-th cluster magnitudo
  ni <- length(index)
  withmean.i <- numeric(ni)
  ### i-th cluster max obs time
  tmax <- max(ClustCurve.i[,2])
  ### i-th cluster meancurve truncated at tmax
  MeanCurve.i <- MeanCurves[,i][which(TimeGrid <= tmax)]
  ### i-th cluster obs time grid
  TimeGrid.i <- TimeGrid[TimeGrid <= tmax]
  ### i-th cluster meancurve
  A <- matrix(c(TimeGrid.i,MeanCurve.i),ncol=2)
  for (k in 1:ni)
  {
    ### i-th cluster curves
    B <- ClustCurve.i[ClustCurve.i[,1]==index[k],2:3]
    ### Hausdorff distance between i-th cluster meancurve and k-th curve respectively
    withmean.i[k] <- hausdorff(A,B)
  }
  return(withmean.i)
}

