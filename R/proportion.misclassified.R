proportion.misclassified <- function(x, na.rm=TRUE) {
  
  if (na.rm==TRUE) {
    x <- x[!is.na(x)]
  } else {
    return(NA)
  }
  
  if (is.factor(x)==FALSE) {
    x <- as.factor(x)
  }
  
  ux <- levels(x)
  K <- length(ux) #No. of categories
  
  fi <- tabulate(match(x, ux)) #frequency of ith group
  N <- sum(fi) #Size of sample
  
  TAE <- sum(abs( fi-(N/K) ) )
  
  iod <- TAE / (2*N)
  
  return(iod)
  
}