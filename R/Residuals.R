#' Calculate residuals, quadratic residuals and the curves fitted both in the observation times and
#' in the grid, that is the union of times of the all curves. It could be done for three models:
#' Malthus, Gompertz and Logistic.
#'
#' @param Model It is the name of the model among Malthus, Gompertz and Logistic that will be used.
#' @param time Vector of the all time for the different curves.
#' @param Vol Vector of observations.
#' @param par Vector of the parameters estimed for the respective model.
#' @param plot If it is "TRUE" then it will be shown a plot od the residuals, an histogram and a qqplot.
#' @return A list in which is stored the residuals, quadratic residuals, the curve fitted in both the
#' observation times and all times.
#' @examples
#'Residuals("Malthus",time,Vol,par,"TRUE")
#'
#' @export
Residuals<- function(model,time,Vol,par,plot)

{
  grid <- sort(unique(time))

  if(model=="Gompertz")
  {
    fitted<- par[1]*exp(-exp((par[2]*exp(1)/par[1])*(par[3]-time)+1))
    res<-fitted-Vol
    qres<- sum((fitted-Vol)^2)

    ######
    curve<-par[1]*exp(-exp((par[2]*exp(1)/par[1])*(par[3]-grid)+1))
  }

  if(model=="Logistic")
  {
    fitted<-par[1]/(1+exp((4*par[2]/par[1])*(par[3]-time)+2))
    res<-fitted-Vol
    qres<- sum((fitted-Vol)^2)

    ######
    curve<-par[1]/(1+exp((4*par[2]/par[1])*(par[3]-grid)+2))
  }

  if(model=="Malthus")
  {
    fitted<- x[1]*exp(x[2]*t)
    res<-Vol - fitted
    qres<-sum(res^2)
    ###
    curve<- x[1]*exp(x[2]*grid)
  }


  if(plot==TRUE)
  {
    windows()
    par(mfrow=c(2,2))
    plot(fitted,res)
    hist(res)
    qqnorm(res)
  }
  return(list("fit"=fitted,"qres"=qres,"curve"=curve,"res"=res))
}
