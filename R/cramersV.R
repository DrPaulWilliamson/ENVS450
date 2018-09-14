#cramersV
cramersV <- function(y, fill = TRUE, na.rm=TRUE){
  
  #Find Cramer's V for each variable pair in the supplied data.frame
  col.y<-ncol(y)
  V <- matrix(ncol=col.y,nrow=col.y)

  for(i in 1:(col.y - 1)){
    for(j in (i + 1):col.y){
      result <- cramersV.test(y[,i],y[,j],na.rm)
      V[i,j] <- result$cramers.v
    }
  }
  
#  Find the diagonal values (1 for categorical values; else NA)
  for(i in 1:col.y){
      result <- cramersV.test(y[,i],y[,i],na.rm)
      V[i,i] <- result$cramers.v
  }

  
  #Complete upper-half of correlation matrix, if required [default = TRUE]
  if (fill) {
    for (i in 1:ncol(V)) {
      V[, i] <- V[i, ]
    }
  }
  V <- data.frame(V)
  row.names(V) <- names(y)
  names(V) <- names(y)
  
  cormat <- V
  
  return(cormat)
  
}
