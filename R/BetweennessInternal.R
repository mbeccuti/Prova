BetweenCluster_CurvDist <- function(ClustSymbol,ClustCurve,i)
{
 ### i-th cluster info
   ### curves data
   ClustCurve.i <- ClustCurve[ClustCurve[,4]==i,]
   ### curves ID
   index.i <- unique(ClustCurve.i[,3])
   ### magnitudo
   ni <- length(index.i)

 ### other clusters info
   ### curves data
   others <- ClustCurve[-which(ClustCurve[,4]==i),]
   ### curves cluster and ID
   clusterID.others <- unique(others[,c(4,3)])
   clusterID.others[,1] <- ClustSymbol[clusterID.others[,1]]
   ### curves ID
   index.others <- clusterID.others[,2]
   ### other clusters magnitudo
   n.others <- length(index.others)
 ### i-th cluster distance matrix
  dist.curve <- matrix(numeric(ni*n.others),ncol=ni)
  colnames(dist.curve) <- paste("curve",index.i,"distance")
  for(j in 1:ni)
    {
     A <- ClustCurve.i[ClustCurve.i[,3]==index.i[j],1:2]
     for(k in 1: n.others)
     {
       B <- others[others[,3]==index.others[k],1:2]
       dist.curve[k,j] <- hausdorff(A,B)
     }
    }

  dist.info <- cbind(clusterID.others,dist.curve)
  other.ncluster <- K-1
  other.cluster <- unique(clusterID.others[,1])

  ### Min between dist.curveance
  betw.m <- numeric(other.ncluster)
  ### Max between dist.curveance
  betw.M <- betw.m

  ### The nearest other cluster curve to i-th cluster
  near.curve <- betw.m
  ### The farest other cluster curve to i-th cluster
  far.curve <- betw.m

  ### Betweenness
  for (p in 1: other.ncluster)
    {
    betw.m[p] <- min(dist.info[dist.info[,1]==sort(other.cluster)[p],-c(1:2)])
    betw.M[p] <- max(dist.info[dist.info[,1]==sort(other.cluster)[p],-c(1:2)])
    }

  ### Nearest and farest curve to i-th cluster
  for (p in 1: other.ncluster)
    {
      near.curve [p] <- index.others[which(dist.curve==betw.m[p],arr.ind=T)[1]]
      far.curve [p] <- index.others[which(dist.curve==betw.M[p],arr.ind=T)[1]]
    }
  between.i <- cbind(betw.m,betw.M)
  rownames(between.i) <- paste("cluster",ClustSymbol[-i])
  colnames(between.i) <- c("betw min","betw max")
  return(list(CurveDistance=dist.info,Between=between.i,NearCurveID=near.curve,FarCurveID=far.curve))
}

BetweenCluster_MeanDist <- function(ClustSymbol,ClustCurve,MeanCurves,i)
{
  TimeGrid <- sort(unique(ClustCurve[,1]))
  ### i-th cluster curves data
  ClustCurve.i <- ClustCurve[ClustCurve[,4]==i,]
  ### i-th cluster max obs time
  tmax <- max(ClustCurve.i[,1])
  ### i-th cluster meancurve truncated at tmax
  MeanCurve.i <- MeanCurves[TimeGrid <= tmax,i]
  ### i-th cluster obs time grid
  TimeGrid.i <- TimeGrid[TimeGrid <= tmax]
  ### i-th cluster meancurve
  A <- cbind(TimeGrid.i,MeanCurve.i)
  ### other clusters
  other.cluster <- sort(unique(ClustCurve[-which(ClustCurve[,4]==i),4]))

  ### Betweenness centroid dist.curveance
  betweencentroid.i <- matrix(numeric(length(other.cluster)),nrow=1)
  count <- 1
  for (j in other.cluster)
   {
    ClustCurve.j <- ClustCurve[ClustCurve[,4]==j,]
    tmax.j <- max(ClustCurve.j[,1])
    MeanCurve.j <- MeanCurves[TimeGrid <= tmax.j,j]
    TimeGrid.j <- TimeGrid[TimeGrid <= tmax.j]
    B <- cbind(TimeGrid.j,MeanCurve.j)
    betweencentroid.i[count] <- hausdorff(A,B)
	count <- count +1
   }
   between <- matrix(numeric(K),nrow=1)
   between[,-i] <- betweencentroid.i
   colnames(between) <- paste(ClustSymbol,"dist")
  return(BetweenCentr=between)
}



