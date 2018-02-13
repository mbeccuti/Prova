#' Cluster Choice
#'
#'@description
#' Cluster_choice() calcolate the BIC and AIC values with respect of different number of cluster (k) and dimension
#' of the cluster mean space (h).
#' The Bayes information criterion (BIC) is a method useful to choose the number of clusters
#' in a certain data sets. Then to rightly estimate the value of the cluster we compare them,
#' choosing the cluster with the minimum BIC value, (same for AIC).
#' Since it requires fitting the model for each potential number of clusters and dimensions of the mean space,
#' each output from the Functional Clustering Method (FCM) is saved in the list FCM_all, per each k and h.
#' Furthermore, insering the PCA percentages calculated with the function PCAbarplot, the dimension h is choosen
#' automatically, so that the sum of the first h percentages is greater than 95 percent.
#'
#'
#' @param databaseTr  List containing the number of observations per each curve (called LenCurv),
#'                and a data frame constituted from the curves' ID, observed values and the respective times,
#'                that might be truncated at a specific time or not.
#'                It is generated automatically from the function DataImport() or DataTruncation() if we want consider
#'                a truncation time.
#' @param K Number of clusters, it could be a vector.
#' @param h Dimension of the cluster mean space.
#'          As default is NULL, so that using the percentages from the PCA it is possible to estimate a value
#'          for it such that the sum of the first h percentages is greater than 95 percent.
#' @param PCAperc The PCA percentages calculated with the function PCAbarplot, if it is NULL (default) then it must
#'        insert in input a value for h.
#' @return List containing the matrices in which are stored the AIC and Bic values for different h and k,
#'         and a list with the FCM's outputs.
#' @examples
#'
#'
#' @import funcy
#' @export
Cluster_choice<-function(databaseTr,K,h=NULL,PCAperc=NULL)
{

  if(is.null(h))
  {
    if(is.null(PCAperc)) print("Choose a value for h or insert the PCA percentages")
      else{ h<-min(which(cumsum(PCAperc)>=95)) }
  }

  H<-1:h

  output_k<-list()
  row_names <-c(paste("k=",K))
  col_names<-c(paste("h=",H))
  matrix_AIC<-matrix(0,nrow = length(K),ncol = length(H),dimnames=list(row_names,col_names))
  matrix_BIC<-matrix(0,nrow = length(K),ncol = length(H),dimnames=list(row_names,col_names))


  data.funcit <-matrix(c(database$ID,database$Vol,database$Time),ncol=3,byrow=F)

  # return a list of K lists, in which is is stored the output for all h
  # We also create two matrixes with the BIC and AIC values
  for(k in K)
  {
    output_h<-list()
    for(h in H)
    {
      mycontfclust = new("funcyCtrl",baseType="splines",dimBase=5,init="kmeans",nrep=10,redDim=h)
      out.funcit<- funcit(data.funcit,seed=2404,k,methods="fitfclust",funcyCtrl=mycontfclust,save.data=TRUE)
      output_h[[paste("h=",h)]] <- out.funcit
      matrix_BIC[which(K==k),which(H==h)]<-output_h[[paste("h=",h)]]@models$fitfclust@BIC
      matrix_AIC[which(K==k),which(H==h)]<-output_h[[paste("h=",h)]]@models$fitfclust@AIC
    }
    output_k[[paste("k=",k)]]<-output_h
  }

  return(list(FCM_all=output_k,matrix_BIC=matrix_BIC,matrix_AIC=matrix_AIC))
}
