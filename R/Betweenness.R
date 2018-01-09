Betweenness <- function(ClustCurve,MeanCurves,centroids)
{ K <- length(unique(ClustCurve[,4]))
  ClustSymbol <- cluster.symbol(K)
  ClassCurve <- unique(ClustCurve[,c(1,4)])[,2]
  classes <- ClustSymbol[ClassCurve]
  if(centroids==FALSE)
  {
   between.near <- matrix(numeric(K*3*(K-1)),ncol=3*(K-1))
   rownames(between.near) <- paste("cluster",ClustSymbol)
   colnames(between.near) <- rep(c("Distance","ID","cluster"),3)
   between.far <- between.near
   for (i in 1:K)
    {
	 Betweenness.i <- BetweenCluster_CurvDist(ClustCurve,i)
	 near.curve <- Betweenness.i$NearCurve
     far.curve <- Betweenness.i$FarCurve
	 for (j in seq(1,3*(K-1),3))
	 count <- 1
	 {
	 between.near[i,j[count]:(j[count]+2)] <- c(t(Betweenness.i$Between[count,1]),near.curve[count],ClustSymbol[ClassCurve[near.curve]][count])
	 between.far[i,j[count]:(j[count]+2)] <- c(t(Betweenness.i$Between[count,2]),far.curve[count],ClustSymbol[ClassCurve[far.curve]][count])}
	 count <- count + 1
	 }
   return(list(NearDist=between.near,FarDist=between.far,Classes=classes))
  }
  
  else
  {
  centroid.dist <- matrix(numeric(K*K),nrow=K)
  between <- matrix(numeric(K*2),ncol=2)
  colnames(between) <- c("Min distance","Nearest cluster")
  rownames(between) <- ClustSymbol
  rownames(centroid.dist) <- ClustSymbol
  colnames(centroid.dist) <- rownames(centroid.dist)
  
  for(i in 1:K)
    {
	centroid.dist[i,] <-  BetweenCluster_MeanDist(ClustCurve,MeanCurves,i) 
	ClustSymbol.i <- ClustSymbol[-i]
	min.dist <- min(centroid.dist[i,][centroid.dist[i,]!=0])
	argmin.dist <- ClustSymbol.i[which.min(centroid.dist[i,][centroid.dist[i,]!=0])]
	between[i,] <- c(min.dist,argmin.dist)
	}
	return(list(Betweenness=between,CentroidDist=centroid.dist,Classes=classes))
  }
}
