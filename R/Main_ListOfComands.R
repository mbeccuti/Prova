library("funcy")
library("grDevices")
library(ggplot2)

file1<-"data/1864dataset.xls"
file2<-"data/1864info.txt"

source("R/DataImport.R")

dati<-DataImport(file1,file2)

source("R/DataTruncJ.R")

dati.tr<-DataTrunc(dati,truncTime=60)

source("R/PCAbarplot.R")

pca<-PCAbarplot(dati.tr$Dataset,save=TRUE)

source("R/cluster_choice.R")

out<-Cluster_choice(dati.tr$Dataset,K=4,h=NULL,pca$perc)

source("R/ClusterWithMeanCurves.R")
out.funcit <-out$FCM_all$`k= 4`$`h= 2`

FCMplots<-ClusterWithMeanCurve_plot(out.funcit,databaseTr = dati.tr,Info = "Progeny",k = 4,All = TRUE,"FCM")

MalthusPlots<-ClusterWithMeanCurve_plot(out.funcit,databaseTr = dati.tr,Info = "Progeny",k = 4,All = TRUE,"Malthus")

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

