#' PCA barplot
#'
#' Plot PCA barplot for a given dataset of tumor growth curves.
#' Columns dataset are: ID, volume and time measures for each curve.
#' print equal TRUE save the plot.
#'
#' @param dataset A data.frame.
#' @param print A logical constant.
#' @return The variances against the number of the principal component are plotted.
#' @examples
#' @import fda
#' @export
PCAbarplot <- function(dataset,print=FALSE)
{
library(fda)
TimeGrid <- 1:max(dataset$time)
res <- makeCoeffs(data=dataset, reg=FALSE, dimBase=5,
                  grid=TimeGrid, pert=0.01,
                  baseType="splines")
princomp(as.matrix(res$coeffs)) -> pca
ncomp <- length(names(pca$sdev))
eigs <- pca$sdev^2
percentage <- eigs/sum(eigs)*100
windows()
screeplot(pca,type="barplot",col="royalblue2",ylim=c(0,11/10*max(eigs)),main="PCA barplot")
text(x=seq(0.2+0.5,ncomp+1+0.2,1+0.2), y=eigs, paste(signif(percentage,4),"%",sep="") ,cex=1,col="red",pos=3)
if(print==TRUE)
{
Sys.sleep(3)
dev.copy2pdf(device = postscript, file = "PCAbarplot.pdf",paper="a4r",width=11)
dev.off()
}
}

makeCoeffs <- function(data, base=NULL, reg, dimBase, grid=NULL, pert, baseType){

    if(is.null(base)){
        tempBase <- makeBasis(baseType, grid, dimBase)$phi
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

makeBasis <- function(basis, time, nbasis){
    m <- length(time)
    switch(basis,
           "splines"={
               bObj <-  create.bspline.irregular(c(time[1],time[m]),
                                                 nbasis=nbasis,
                                                 norder=min(nbasis, 4))
           }
           )
    phi <- eval.basis(time, bObj)
    return(list(bObj=bObj, phi=phi))
}
