PDXGrowthCurve <- function(dati,name.sample,style,tr,labeled,truncated) # style = plot type
{

source("auxiliarfunctions.R")
topi <- dati$topi  
progenie <- dati$progenie
curve <- dati$curve
time <- dati$time
observations <-dati$x
generazioni <- unique(substring(dati$topi,nchar(dati$topi)-2,nchar(dati$topi)-1))
nomi <- substring(topi,nchar(topi)-2,nchar(topi))
ntopi <- length(dati$topi)

maxvol=max(observations)
maxtime=max(time)
ntrees= max(progenie)
colors <- dati$generazione


tit <- c(" PDXGrowthCurve ")
tit <- paste(tit,name.sample,sep="")
if(style != "l") {file.name <- paste(tit,"points")}
file.name <- paste(tit,".pdf")
#pdf(file =file.name, paper = "a4r", width = 11)

if(style=="l")
{ t <- time[which(curve==1)]
  obs <- observations[curve==1]
  plot(t,obs,col=colors[1],type="l",xlab='Days',ylab='Volume',cex=1.5, xlim=c(0,maxtime), ylim=c(0,maxvol),lwd=2)
  if(labeled==TRUE) {text(x=max(t),y=obs[length(obs)],labels=nomi[1],cex=0.6, pos=4, col="black")}  

for (i in 2:ntopi) {
  t <- time[which(curve==i)]
  obs <- observations[curve==i]
  lines(t,obs, col=colors[i],cex=1.5,lwd=2)
  if(labeled==TRUE) {text(x=max(t),y=obs[length(obs)],labels=nomi[i],cex=0.6, pos=4, col="black")}
}
  
 legend("topleft", generazioni, lty=1,col=dati$colori[1:ntrees], title="Legend", bty="n", cex=1,lwd=2)
}


else
{ plot(time[which(curve==1)],observations[which(curve==1)],col=colors[1],xlab='Time',ylab='Volume',cex=1, xlim=c(0,maxtime), ylim=c(0,maxvol),lwd=2,pch=16)
  
  
  for (i in 2:ntopi) {
    t <- time[which(curve==i)]
    obs <- observations[curve==i]
    points(t,obs, col=colors[i],lwd=2,pch=16)
    legend("topleft", generazioni, col=dati$colori[1:ntrees], title="Legend", bty="n", cex=1,pch=16)
  }
  
  
 
}
  

if(truncated==TRUE) abline(v=tr,lwd=3.5)


title(tit)


#dev.off()

}