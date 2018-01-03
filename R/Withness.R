Withness <- function(ClustSymbol,ClustCurve,MeanCurves,K,centroids)
{ 
  ### Withness matrix
  withness <- matrix(numeric(K*2),ncol=K)
  rownames(withness) <- c("mean","sd")
  colnames(withness) <- paste("Cluster ",ClustSymbol,sep="")
  
  for (i in 1:K)
  {
    if(centroids==FALSE) ### i-th cluster curves distance
	{ 
	  within.i <- WithCluster_CurvDist(ClustCurve,i)
	}
    else                 ### i-th cluster curves and meancurve distance      
	  within.i <- WithCluster_MeanDist(ClustCurve,MeanCurves,i) 
	
    ### i-th cluster mean distance	
    MeanDist <- mean(within.i)
	### i-th cluster standard deviation distance
    if(length(within.i)==1) { StDev <- 0 }
    else                        StDev <- sd(within.i)
	### i-th cluster withness data 
    withness[,i] <- cbind(MeanDist,StDev)
  }
 return(withness)
}