#kurtosis() - function to return kurtosis, and cope with filtering out missing (NA) values
kurtosis <-  function(x, na.rm=TRUE) {
  #Near direct copy of solution posted by Wolfgang Koller
  #https://stat.ethz.ch/pipermail/r-help/1999-July/004529.html
  if (na.rm==TRUE) {
    x <- x[!is.na(x)]
  }
  m4 <- mean((x-mean(x))^4)
  kurtosis <- m4/(sd(x)^4)
  return(kurtosis)
}