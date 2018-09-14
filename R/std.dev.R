std.dev <- function(x, na.rm=TRUE, sample=TRUE) {
  
  if (na.rm==TRUE) {
    x <- x[!is.na(x)]
  }
  
  SE <- (x-mean(x))^2
  n <- length(x)
  if (sample==TRUE) {
    n <- n - 1
  }
  MSE <- sum(SE) / n
  RMSE <- MSE^0.5
  return(RMSE)
}
