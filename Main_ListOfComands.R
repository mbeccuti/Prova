library(Prova1)

### Data files
GrowDataFile<-"data/1864dataset.xls"
AnnotationFile <-"data/1864info.txt"

### Merge curves and target file
CONNECTORList <- DataImport(GrowDataFile,AnnotationFile)

### Visualization
DataVisualization(CONNECTORList,feature="Progeny",labels = c("time","volume","Tumor Growth"),save=TRUE,path="~/Desktop/ImagesPerFrancesca/")
CONNECTORList<- DataTruncation(alldata=CONNECTORList,feature="Progeny",truncTime=60,save=TRUE,path="~/Desktop/ImagesPerFrancesca/",labels = c("time","volume","Tumor Growth"))

### PCA
pca <- PCA.Analysis(data.tr$Dataset,save=TRUE,path="~/Desktop/ImagesPerFrancesca/")
pca$plot

k <- c(2:6)

CONNECTORList.FCM <- Cluster_choice(CONNECTORList,K=c(2:6),h=2)

out.funcit <- CONNECTORList.FCM$FCM_all$`k= 4`$`h= 2`

FCMplots <- ClusterWithMeanCurve(out.funcit,CONNECTORList,k = k,"FCM",feature = "Progeny")

MalthusPlots<- ClusterWithMeanCurve(database = CONNECTORList,k = 4,model="Malthus",feature = "Progeny")

### Withinness and betweenness plot for FCM
ClustCurve <- FCMplots$Information$ClustCurve
MeanCurves <- FCMplots$Information$meancurves
PlotWithinnessBetweenness(ClustCurve,MeanCurves,save=TRUE,path="~/Desktop/ImagesPerFrancesca/")

fitclust_allmodels <- FittingAndClustering(databaseTr = data.tr, h = 2, k=k,FCM_all =
out$FCM_all, feature = "Progeny",save=TRUE,path="~/Desktop/ImagesPerFrancesca/",labels = c("time","volume"))

list_conteggio<-CountingSamples(fitclust_allmodels,Model = "Malthus")

