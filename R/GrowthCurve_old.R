GrowthCurve <- function(dataset,name.sample,feature,tr.day=NULL)
{

feat        <- dataset$Labels[,feature]
ID          <- dataset$data.matrix[,1]
times       <- dataset$data.matrix[,3]
volume      <- dataset$data.matrix[,2]
sample.size <- length(unique(ID))
labgroup    <- unique(feat)

maxvol      <- max(volume)
maxtimes    <- max(times)
nfeat       <- length(unique(feat))


# plot 1^st curve
  t         <- times[which(ID==1)]
  vol       <- volume[ID==1]
  windows()
  plot(t,vol,col=colour[feat[1]],type="l",xlab='Days',ylab='Volume',cex=1.5, xlim=c(0,maxtimes), ylim=c(0,maxvol),lwd=2)
 # if(labeled==TRUE) {text(x=max(t),y=vol[length(vol)],labels=feat[1,2],cex=0.6, pos=4, col="black")

# plot remaining curves
for (i in 2:sample.size) {
  t        <- times[which(ID==i)]
  vol      <- volume[ID==i]
  lines(t,vol, col=colour[feat[i]],cex=1.5,lwd=2)
}

# make legend
 legend("topleft", labgroup, lty=1,col=colour, title="Legend", bty="n", cex=1,lwd=2)


# make line to truncate curves
if(!is.null(tr.day)) abline(v=tr.day,lwd=3.5)

# make title
title(tit)

   {
   Sys.sleep(3)
   dev.copy2pdf(device = postscript, file = "GrowthCurve.pdf",paper="a4r",width=11)
   dev.off()
   }

}
