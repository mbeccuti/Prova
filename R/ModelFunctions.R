
Bertalanffy <- function(x,t)
{
  (x[2]/x[3]+(x[1]^(1-x[4])-x[2]/x[3])*exp(-x[3]*(1-x[4])*t))^(1/(1-x[4]))
}
lsBertalanffy <- function(x,t,y)
{
  sum((Bertalanffy (x,t)-y)^2)
}
fitBertalanffy <- function(a,b,gamma,time,Vol)
{
  initBertalanffy(time,Vol,A, mu, lambda) -> initgomp
  initBertalanffy<- list(A=initgomp$A,mu=initgomp$mu,lambda=initgomp$lambda)
  fitBertalanffy <- optim(initBertalanffy,lsBertalanffy,method="L-BFGS-B",lower=c(0,0,0),t=time,y=Vol)
  return(fitgomp$par)
}


#####
gompertz <- function(x,t)
{
  (x[1]*exp(-exp((x[2]*exp(1)/x[1])*(x[3]-t)+1)))
}
lsgompertz <- function(x,t,y)
{
  sum((gompertz(x,t)-y)^2)
}


logistic <- function(x,t)
{
  (x[1]/(1+exp((4*x[2]/x[1])*(x[3]-t)+2)))
}

malthus <- function(x,t)
{
 x[1]*exp(x[2]*t)
}

lslogistic <- function(x,t,y)
{
  sum((logistic(x,t)-y)^2)
}

lsmalthus <- function(x,t,y)
{

  sum((malthus(x,t)-y)^2)
}
