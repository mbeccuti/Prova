PlotWithinnessBetweenness <- function(ClustCurve,MeanCurves,path)
{
  library(ggplot2)
  library(ggforce)
  K <- length(unique(ClustCurve[,4]))
  ClustSymbol <- cluster.symbol(K)
  nfeature <- length(unique(ClustCurve[,5]))[1]
  feature.palette <- rainbow(nfeature+3)
  feature.lev <- sort(unique(ClustCurve[,5]))
  within.all <- Withinness(ClustCurve,MeanCurves,centroids=TRUE)
  between.all <- Betweenness(ClustCurve,MeanCurves,centroids=TRUE)$Betweenness
  linked <- cbind(ClustSymbol,NearestClust=between.all[,2])
  linked <- t(apply(linked,1,sort))
  betweenness <- between.all[duplicated(linked)==FALSE,]
  shift <- c(0,betweenness[,1])
  i <- rep(1,K)

  for(j in 1:(K-1))
  {
   if(!(ClustSymbol[i[j]] %in% rownames(betweenness)))
    {
    ### i[j] th cluster neighbors
    neighbors <- rownames(betweenness)[betweenness[,2]==ClustSymbol[i[j]]]
    i[j+1] <- which(ClustSymbol==neighbors[!(neighbors %in% ClustSymbol[i])])
    }
   else i[j+1] <- which(ClustSymbol==betweenness[i[j],2])
  }
  cluster.magnitudo <- apply(table(unique(ClustCurve[,c(4,1)])),1,sum)
  circles <- data.frame(
  x0 = numeric(3*K)     ,
  y0 = numeric(3*K)     ,
  r = numeric(3*K)      ,
  distance = numeric(3*K),
  Cluster = numeric(3*K)
  )  
  colnames(circles) <- c("x0","y0","r","distance","Cluster")
  n <- length(unique(ClustCurve[,1]))
  WithDist <- data.frame(
  x1 = numeric(n),
  y1 = numeric(n),
  Cluster = numeric(n),
  feature = numeric(n)
	)  
  colnames(WithDist) <- c("x1","y1","Cluster","feature")
  counter <- 1
  index <- matrix(seq(1,3*K),nrow=K,byrow=TRUE)
  for(k in 1:K)
  {
  ### Data frames
  dataplot <- DataFrameWithiness.i(ClustCurve,MeanCurves,i[k],centroids=TRUE,shift=cumsum(shift)[i[k]])
  circles[index[k,],] <- dataplot$circles
  WithDist[counter:(cumsum(cluster.magnitudo)[i[k]]),] <- dataplot$WithDist
  counter <- cumsum(cluster.magnitudo)[i[k]] + 1
  }
  
  circles$distance <- factor(circles$distance)
  circles$Cluster <- factor(circles$Cluster)
  WithDist$Cluster <- factor(WithDist$Cluster)
  WithDist$feature <- factor(WithDist$feature)

  plots <- ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r,linetype=distance), data=circles,size=1,show.legend=FALSE)
  plots <- plots + scale_shape_manual(values=c(0:(K-1))) + geom_point(data=WithDist,aes(x=x1,y=y1,shape=Cluster,color=feature),size=2)
  feature.name <- colnames(WithDist)[4]
  plots <- plots  + geom_text(aes(x=x0, y=y0,label=Cluster), data=circles) + scale_colour_manual(values = feature.palette,name=feature.name) + labs(title="Cluster betweenness and withinness",x="distance",y="distance")
  pdf(paste(path,"Betweenness&Withinness.pdf",sep=""),paper="a4r",width=11)
  print(plots)
  dev.off()
  return(plots)
}

PlotWithinness.all <- function(ClustCurve,MeanCurves,centroids=TRUE,shift=0)
{
  library(ggplot2)
  K <- length(unique(ClustCurve[,4]))
  ClustSymbol <- cluster.symbol(K)
  plots <- vector("list",K)
  maxdist <- max(Withinness(ClustCurve,MeanCurves,centroids)[4,])
  axislim <- c(-maxdist,maxdist)
  for(i in 1:K) plots[[i]] <- PlotWithinness.i(ClustCurve,MeanCurves,i,centroids,shift)+ coord_cartesian(xlim = axislim, ylim = axislim)
  return(plots)
}

 DataFrameWithinness.i <- function(ClustCurve,MeanCurves,i,centroids=centroids,shift=shift)
{
  K <- length(unique(ClustCurve[,4]))
  ClustSymbol <- cluster.symbol(K)
  feature <- colnames(ClustCurve)[5]
  nfeature <- length(unique(ClustCurve[,feature]))[1]
  featurecurve.i <- t(unique(ClustCurve[ClustCurve[,4]==i,c(1,5)])[2])
  if(centroids==FALSE)
  {
   Withinness.i <- WithCluster_CurvDist(ClustCurve,i)
   }
  else
  {
   Withinness.i <- WithCluster_MeanDist(ClustCurve,MeanCurves,i)
  }
  ### Mean and standard deviation distance
  mean.dist <- mean(Withinness.i)
  st.dev <- sd(Withinness.i)

  ### Data frame for ggplot
  circles.i <- data.frame(
  x0 <- rep(0+shift,3),
  y0 <- rep(0,3),
  r <- rep(mean.dist, 3)+c(st.dev,0,-st.dev),
  distance <- c("sd","mean","sd"),
  Cluster <- factor(ClustSymbol[i],levels=ClustSymbol)
  )
  colnames(circles.i) <- c("x0","y0","r","distance","Cluster")

  WithDist.i <- data.frame(
  x1 <- Withinness.i + shift,
  y1 <- numeric(length(Withinness.i)),
  Cluster <- factor((i-1),levels=c(0:(K-1))),
  type <- factor(featurecurve.i,levels=c(1:nfeature))
  )
  colnames(WithDist.i) <- c("x1","y1","Cluster","feature")
 return(DataFrame.i=list(circles=circles.i,WithDist=WithDist.i))
 }

PlotWithinness.i <- function(ClustCurve,MeanCurves,i,centroids=TRUE,shift=0)
{
  library(ggforce)
  dataplot <- DataFrameWithinness.i(ClustCurve,MeanCurves,i,centroids=centroids,shift=shift)
  ### Data frame for plot
  circles <- dataplot$circles
  WithDist <- dataplot$WithDist
  feature <- WithDist[,3]
  feature.name <- colnames(WithDist)[3]
  nfeature <- length(unique(ClustCurve[,feature.name]))[1]
  feature.palette <- rainbow(nfeature)
  ### Plot distances
  plot.i <- ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r), data=circles,size=1)
  plot.i <- plot.i  + geom_point(data=WithDist,aes(x=x1,y=y1,col=feature),shape=(i-1)) + labs(title=paste("Cluster",ClustSymbol[i],"withinness"),x="distance",y="distance")
  plot.i <- plot.i + scale_colour_manual(values = feature.palette[unique(feature)],name=feature.name) + xlab("distance x") + ylab("distance y")
  return(plot.i)
 }

