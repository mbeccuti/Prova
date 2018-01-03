library("funcy")
library("grDevices")
library("ggplot2")


file1<-"data/1864dataset.xls"
file2<-"data/1864info.txt"

source("R/DataImport.R")

dati<-DataImport(file1,file2)

source("R/DataTrunc.R")

dati.tr<-DataTrunc(dati,60)

source("R/PCAbarplot.R")

pca<-PCAbarplot(dati.tr$data.matrixtr,save=TRUE)

source("R/cluster_choice.R")

out<-Cluster_choice(dati.tr,K=4,h=NULL,pca$perc)

source("R/ClusterWithMeanCurves.R")
out.funcit <-out$FCM_all$`k= 4`$`h= 2`

FCMplots<-ClusterWithMeanCurve_plot(out.funcit,databaseTr = dati.tr,Info = "Progeny",k = 4,All = TRUE,"FCM")

MalthusPlots<-ClusterWithMeanCurve_plot(out.funcit,databaseTr = dati.tr,Info = "Progeny",k = 4,All = TRUE,"Malthus")

source("R/FittingAndClustering.R")

ciao<-FittingAndClustering(databaseTr = dati.tr, h = 2, k=4,FCM_all = out$FCM_all, Info = "Progeny")

