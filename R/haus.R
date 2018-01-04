# Hausdorff distance

eucl.dist <- function(x) {sqrt(sum(x^2))}
# A, B vettori 
distance <- function(A,B) 
{
   n <- nrow(A)
   m <- nrow(B)
   a <- numeric(n)
   b <- numeric(m)
   
   for (i in 1:n)
      {
        for (j in 1:m)
        {
          b[j] <- eucl.dist(A[i,]-B[j,])
        }
          a[i] <- min(b)
      } 
   return(max(a))
}

hausdorff <- function(A,B)
{
  H <- max(distance(A,B),distance(B,A))
  return(H)
}