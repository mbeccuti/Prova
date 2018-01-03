#' To write.......
#'
#'
#' @param Connector the list in which is stored all the informations, output from the function:"DataInfo"
#' @param number_curves value that represents the number of the curves.
#' @param k the value of the cluster that you want consider.
#' @param model is the model that you are considering and with respect to you want to cluster
#' @return A list of 3 elements, that are the parameters of the fitting, the center and the cluster of
#'         affinity, for each curve.
#' @examples
#'
#'
#' @import grofit
#' @export
library(grofit)
source("R/ModelFunctions.R")
clustering<- function(databaseTr,k,model)
{

  databaseTr->dati
  time <- dati$data.matrixtr$Time
  t <- sort(unique(time))
  x <- dati$data.matrixtr$Vol
  curve <- dati$data.matrixtr$ID
  number_curves<-max(curve)

  data.fit <- matrix(c(curve,x,time),ncol=3,byrow=F)
  parameters <- matrix(0,ncol=3,nrow=number_curves)
  meancurves <- matrix(numeric(length(t)*k),ncol=k)

  if(model=="Malthus")
  {
    parameters <- matrix(0,ncol=2,nrow=number_curves)
  }

  for (i in c(1:number_curves))
  {
    m <- which(data.fit[,1] == i)
    tdata <- time[m]
    Voldata <- x[m]


    # fitgompertz
    if(model=="Gompertz")
    {
      initgompertz(tdata,Voldata,1,1,1) -> initgomp
      initgomp<- list(A=initgomp$A,mu=initgomp$mu,lambda=initgomp$lambda)
      parameters[i,]<- optim(initgomp,lsgompertz,method="L-BFGS-B",lower=c(0,0,0),t=tdata,y=Voldata)$par
    }

    # fitlogistic
    if(model=="Logistic")
    {
      initlogistic(tdata,Voldata,1,1,1) -> initlog
      initlog<- list(A=initlog$A,mu=initlog$mu,lambda=initlog$lambda)
      parameters[i,]<- optim(initlog,lslogistic,method="L-BFGS-B",lower=c(0,0,0),t=tdata,y=Voldata)$par

    }

    # fitMalthus
    if(model=="Malthus")
    {
      par <- matrix(numeric(200),ncol=2)
      res <- numeric(100)
      for(j in 1:100)
      {
        fitmal <- optim(abs(rnorm(2)),lsmalthus,t=tdata,y=Voldata)
        par[j,] <- fitmal$par
        res[j] <- sum(abs(malthus(par[j,],tdata)-Voldata)^2)
      }
      pos <- which(res==min(res))
      parameters[i,]<- par[pos,]
    }
  }

  set.seed(2404)
  group <- kmeans(parameters,k)
  cluster <- group$cluster
  center <- group$centers

###### meancurves calculating ######

  for(i in c(1:k))
  {
    center[i,]->x
    if(model=="Gompertz")
      {
        meancurves[,i] <- (x[1]*exp(-exp((x[2]*exp(1)/x[1])*(x[3]-t)+1)))
      }
    if(model=="Logistic")
      {
        meancurves[,i] <- (x[1]/(1+exp((4*x[2]/x[1])*(x[3]-t)+2)))
      }
    if(model=="Malthus")
      {
        meancurves[,i] <- x[1]*exp(x[2]*t)
      }
  }

  out<-list(parameters=parameters,cluster=cluster,center=center,meancurves=meancurves)
  return(out)
}
