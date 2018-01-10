library("funcy")
library("grDevices")
library("ggplot2")
library("readxl")
library("cowplot")

file1<-"data/1864dataset.xls"
file2<-"data/1864info.txt"

source("R/DataImport.R")

dati<-DataImport(file1,file2)

source("R/TimeGridDensity.R")

TimeGridDensity(dati,save=TRUE,path=NULL)

source("R/GrowthCurve.R")
source("R/DataTruncation.R")
source("R/DataVisualization.R")

dati.tr <- DataTruncation(alldata=dati,trunc.time=60,feature="Progeny",save=TRUE,path="data")

DataVisualization(dati,feature="Progeny",save=TRUE,path="data")

source("R/PCAbarplot.R")
pca<-PCAbarplot(dati.tr$Dataset,save=TRUE,path="data")

source("R/cluster_choice.R")

k=4

out<-Cluster_choice(dati.tr$Dataset,K=k,h=2)

source("R/ClusterWithMeanCurves.R")
out.funcit <-out$FCM_all$`k= 4`$`h= 2`

FCMplots<-ClusterWithMeanCurve(out.funcit,databaseTr = dati.tr,feature = "Progeny",k = k,"FCM")

MalthusPlots<-ClusterWithMeanCurve(out.funcit,databaseTr = dati.tr,feature = "Progeny",k = k,"Malthus")

source("R/FittingAndClustering.R")

ciao<-FittingAndClustering(databaseTr = dati.tr, h = 2, k=k,FCM_all = out$FCM_all, feature = "Progeny",save=TRUE,path="data")

source("R/haus.R")
source("R/cluster.symbol.R")

### Withinness
source("R/WithinnessInternal.R")
ClustCurve <- FCMplots$Information$ClustCurve
MeanCurves <- FCMplots$Information$meancurves

source("R/Withinness.R")
Withinness(ClustCurve,MeanCurves,centroids=FALSE) -> wt

### Betweenness
source("R/BetweennessInternal.R")
BetweenCluster_CurvDist(ClustCurve,i)
BetweenCluster_MeanDist(ClustCurve,MeanCurves,i)
source("R/Betweenness.R")

curve.class <- FCMplots$Information$classes
Betweenness(ClustCurve,MeanCurves,centroids=TRUE)

### Plot
source("R/PlotWithinnessBetweenness.R")
PlotWithinness.i(ClustCurve,MeanCurves,i,centroids=TRUE,shift=0)
PlotWithinnessBetweenness(ClustCurve,MeanCurves,path="R/") -> t

