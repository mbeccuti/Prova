library(Prova1)

### Data files
curve_file <-"data/1864dataset.xls"
target_file <-"data/1864info.txt"

### Merge curves and target file
data <- DataImport(curve_file,target_file)

### Visualization
DataVisualization(data,feature="Progeny",save=FALSE,path=NULL)
data.tr <- DataTruncation(alldata=data,feature="Progeny",truncTime=60)

### PCA
pca <- PCAbarplot(data.tr$Dataset,save=FALSE,path=NULL)

k <- 4

out <- Cluster_choice(data.tr$Dataset,K=k,h=2)

out.funcit <- out$FCM_all$`k= 4`$`h= 2`

FCMplots <- ClusterWithMeanCurve(out.funcit,databaseTr = data.tr,k = k,"FCM",feature = "Progeny")

MalthusPlots<- ClusterWithMeanCurve(out.funcit,databaseTr = data.tr,k = k,"Malthus")

### Withinness and betweenness plot for FCM
ClustCurve <- FCMplots$Information$ClustCurve
MeanCurves <- FCMplots$Information$meancurves
PlotWithinnessBetweenness(ClustCurve,MeanCurves)

fitclust_allmodels <- FittingAndClustering(databaseTr = data.tr, h = 2, k=k,FCM_all = out$FCM_all, save=FALSE,path=NULL)

list_conteggio<-CountingSamples(fitclust_allmodels,Model = "Malthus")

