#' Principal Components Analysis bar plot
#'
#' Add a dataset of ID, vol and time measures and choose to save or not the PCA bar plot.
#'
#' @param data.matrix A matrix with 3 columns: curve ID, volume and time measures.
#' @param save A logical constant.
#' @return The plot of the variances against the number of the principal component.
#' @examples
#' @import fda
#' @export
PCAbarplot <- function(data.matrix,save=FALSE)
{
 library(fda)

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
    ggsave(filename="PCAbarplot.pdf",plot =PCA_barplot,width=29, height = 20, units = "cm",scale = 1 )
  }
  return(list(plot=PCA_barplot,perc=percentage))
  }

# makeCoeffs function returns the data coefficients with respect to a base type chosen
makeCoeffs <- function(data, base=NULL, reg, dimBase, grid=NULL, pert){

    if(is.null(base)){
        tempBase <- makeBasis(grid, dimBase)$phi
        base <- svd(tempBase)$u
    }else{
        base <- base[,1:dimBase]
    }
    if(reg){
        coeffs <- t((solve(t(base) %*% base + pert *
                               diag(dimBase))
                     %*%t(base))%*%t(data))
        fullBase <- base
    }else{
        curveIndx <- data[,1]
        timeIndx <- match(data[,3],grid)
        n <- max(curveIndx)
        fullBase <- base[timeIndx,  ]
        coeffs <- matrix(0,nrow=n,ncol=sum(dimBase))
        for (i in 1:n){
            if(is.null(dim(base)[1]))
                base <- t(t(base))
            basei <- fullBase[curveIndx==i,]
            yi <- data[curveIndx==i,2]
            if(length(yi)>1){
                coeffs[i,] <- solve(t(basei) %*% basei + pert * diag(dimBase)) %*% t(basei) %*%yi
            }else{
                coeffs[i,] <- ((basei) * basei + pert )^(-1) * (basei)*yi
            }
        }
    }
    return(list(coeffs=coeffs, base=base, fullBase=fullBase, dimBase=dimBase))
}

# makeBasis function returns a splines basis, chosen a time grid and a basis dimension
makeBasis <- function(time, nbasis){
    m <- length(time)
    bObj <-  create.bspline.irregular(c(time[1],time[m]),
                                                 nbasis=nbasis,
                                                 norder=min(nbasis, 4))
    phi <- eval.basis(time, bObj)
    return(list(bObj=bObj, phi=phi))
}
