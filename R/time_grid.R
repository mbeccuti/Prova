path <- "D:/Versione stabilizzata/Code/Funzioni ausiliarie/475_no_P2a_P2b.csv"
path <- "D:/Versione stabilizzata/Code/Funzioni ausiliarie/1864_no_P7.csv"
tr.day <- 60
nmice_475 <- 17
nmice_1864 <- 24
creazionedati(path,nmice_1864,tr.day) -> all
creazionedati(path,nmice_475,tr.day) -> all
time.grid <- function(curves,times,length,save=TRUE)
{
db <- cbind(all$curve,all$time)
time.grid <- matrix(numeric(all$lunghezze%*%all$lunghezze*2),ncol=2)
counter <- 0
for (i in 1:max(unique(all$curve)))
  {
    t <- db[,2][db[,1]==i]
    n <- length(t)
    time.grid[(counter + 1):(counter+(n*n)),] <- cbind(rep(t,n),rep(t,each=n))
    counter <- counter + n*n
}
windows()
plot(time.grid,xlab="time",ylab="time",main="Time grid")
}
#???matrix(as.numeric(unlist(strsplit(outer(a,a,function(x,y) paste(x,y,sep=",")), ","))),ncol=2,byrow=TRUE)
