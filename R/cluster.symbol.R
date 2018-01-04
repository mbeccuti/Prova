cluster.symbol <- function(K)
{
symbol.matrix <- cbind(paste("pch=",seq(0:14),sep=""),
c("square","circle","triangle point up","plus","cross","diamond","triangle point down","square cross","star","diamond plus","circle plus","triangles up and down","square plus","circle cross","square and triangle down"))
colnames(symbol.matrix) <- c("pch","symbol")
symbol.name <- symbol.matrix[1:K,2]
return(symbol.name) 
}