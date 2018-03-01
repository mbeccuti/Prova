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

CONNECTORList.FCM.k4.h2<- CONNECTORList.FCM$FCM_all$`k= 4`$`h= 2`

FCMplots <- ClusterWithMeanCurve(CONNECTORList.FCM.k4.h2,CONNECTORList,k = 4,"FCM",feature = "Progeny",labels = c("Time","Volume"))

MalthusPlots<- ClusterWithMeanCurve(database = CONNECTORList,k = 4,model="Malthus",feature = "Progeny")

CONNECTORList.models <- FittingAndClustering(databaseTr = CONNECTORList, FCM_all = CONNECTORList.FCM, h = 2, k=3, feature = "Progeny", labels = c("time","volume"))

### Withinness and betweenness plot for FCM
Malthus.ClustCurve <-  CONNECTORList.models$Malthus$Information$ClustCurve
Malthus.MeanCurves <-  CONNECTORList.models$Malthus$Information$meancurves
PlotWithinnessBetweenness(Malthus.ClustCurve,Malthus.MeanCurves,Title = "Malthus Cluster betweenness and withinness")

Logistic.ClustCurve <-  CONNECTORList.models$Logistic$Information$ClustCurve
Logistic.MeanCurves <-  CONNECTORList.models$Logistic$Information$meancurves

PlotWithinnessBetweenness(Logistic.ClustCurve,Logistic.MeanCurves,Title = "Logistic Cluster betweenness and withinness")

Gompertz.ClustCurve <-  CONNECTORList.models$Gompertz$Information$ClustCurve
Gompertz.MeanCurves <-  CONNECTORList.models$Gompertz$Information$meancurves

PlotWithinnessBetweenness(Gompertz.ClustCurve,Gompertz.MeanCurves,Title = "Gompertz Cluster betweenness and withinness")

FCM.ClustCurve <-  CONNECTORList.models$FCM$Information$ClustCurve
FCM.MeanCurves <-  CONNECTORList.models$FCM$Information$meancurves

PlotWithinnessBetweenness(FCM.ClustCurve,FCM.MeanCurves,Title = "FCM Cluster betweenness and withinness")

fitclust_allmodels <- FittingAndClustering(databaseTr = data.tr, h = 2, k=k,FCM_all =
out$FCM_all, feature = "Progeny",save=TRUE,path="~/Desktop/ImagesPerFrancesca/",labels = c("time","volume"))



NumberSamples<-CountingSamples(CONNECTORList.models)

