library("funcy")
library("grDevices")
library("ggplot2")


file1<-"data/1864dataset.xls"
file2<-"data/1864info.txt"

source("R/DataImport.R")

dati<-DataImport(file1,file2)

source("R/DataTruncJ.R")

dati.tr<-DataTrunc(dati,truncTime=60)

source("R/PCAbarplot.R")

pca<-PCAbarplot(dati.tr$Dataset,save=TRUE)

source("R/cluster_choice.R")

k=4

out<-Cluster_choice(dati.tr$Dataset,K=k,h=NULL,pca$perc)

source("R/ClusterWithMeanCurves.R")
out.funcit <-out$FCM_all$`k= 4`$`h= 2`

FCMplots<-ClusterWithMeanCurve_plot(out.funcit,databaseTr = dati.tr,feature = "Progeny",k = k,All = TRUE,"FCM")

MalthusPlots<-ClusterWithMeanCurve_plot(out.funcit,databaseTr = dati.tr, feature = "Progeny",k = k,All = TRUE,"Malthus")

source("R/FittingAndClustering.R")

ciao<-FittingAndClustering(databaseTr = dati.tr, h = 2, k=k,FCM_all = out$FCM_all, feature = "Progeny")

source("R/haus.R")
source("R/cluster.symbol.R")

### Withness
source("R/WithnessInternal.R")
K <- 4
ClustCurve <- FCMplots$ClustCurve
MeanCurves <- FCMplots$Informations$meancurves
ClustSymbol<-cluster.symbol(K)
source("R/Withness.R")
Withness(ClustSymbol,ClustCurve,MeanCurves,K,centroids=FALSE)

### Betweenness
source("R/BetweennessInternal.R")
BetweenCluster_CurvDist(ClustCurve,i)
BetweenCluster_MeanDist(ClustCurve,MeanCurves,i)
source("R/Betweenness.R")
curve.class <- FCMplots$Informations$classes
Betweenness(ClustSymbol,ClustCurve,MeanCurves,curve.class,K,centroids=FALSE)

