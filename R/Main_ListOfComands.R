library("funcy")
library("grDevices")
library("ggplot2")

file1<-"data/1864dataset.xls"
file2<-"data/1864info.txt"

source("R/DataStructure.R")

dati<-DataStructure(file1,file2)

source("R/DataTrunc.R")

dati.tr<-DataTrunc(dati,60)

source("R/PCAbarplot.R")

pca<-PCAbarplot(dati.tr$data.matrixtr,save=TRUE)

source("R/cluster_choice")



