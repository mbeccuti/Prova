#' PCA Bar Plot
#'
#'@description
#' PCAbarplot() is used to choice the dimension of the cluster mean space, h.
#' The PCA method performs an accurate analysis to determine whether the means lie in a lower
#' dimensional space, note that if h is equal to the number of cluster menus one, does not produce restriction on the mean
#' space.
#' To estimate h the Principal Component Analysis (PCA) is applied to the spline coefficients provided
#' for each curves by the functional clustering model.
#' It generates a bar plot indicating with how much percentage the principal components explain
#' the variability in the data.
#' Usually the components are choosen if the sum of the respective percentages is greater than 95 percent.
#'
#'
#' @param data.matrix Matrix with 3 columns: curve ID, volume and time measures.
#' @param save When TRUE (the default is FALSE), it is possible to save a plot that compares the density time grid and
#'             the growth curves plot in a pdf.
#' @param path Path to save plot to (combined with filename).
#' @return List containing the plot of the variances against the number of the principal component and
#'         the vector of percentages.
#' @examples to write...
#' @import ggplot2
#' @export
PCAbarplot <- function(data.matrix,save=FALSE,path=NULL)
{
  source("R/makeCoeffs.R")
  TimeGrid <- c(1:max(data.matrix[,3]))

  # curves splines basis coefficients
  res <- makeCoeffs(data=data.matrix, reg=FALSE, dimBase=5,
                     grid=TimeGrid, pert=0.01)

  # Principal Components Analysis

  princomp(as.matrix(res$coeffs)) -> pca
  # Number of principal components
  ncomp <- length(names(pca$sdev))
  # Principal components variances
  eigs <- pca$sdev^2
  # Percentage of variances explained by each component
  percentage <- eigs/sum(eigs)*100

  # PCA bar plot
  PCA_barplot<-ggplot(data=data.frame(comp=paste("Comp.",1:ncomp),Variances=eigs,perc=paste(signif(percentage,4),"%",sep="")), aes(x=comp, y=Variances)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=perc), vjust=-.3,  size=3.5)+
    labs(title="PCA barplot", x="Components", y = "Variances")+
    theme(plot.title = element_text(hjust = 0.5))

  if(save==TRUE)
  {
    ggsave(filename="PCAbarplot.pdf",plot =PCA_barplot,width=29, height = 20, units = "cm",scale = 1,path = path)
  }
  return(list(plot=PCA_barplot,perc=percentage))
  }

